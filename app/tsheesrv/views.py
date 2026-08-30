import io
import json
from urllib.parse import unquote_plus
import aiohttp
from aiohttp import web
import logging

from app.tsheesrv.models import (
    send_timesheet as db_send_timesheet,
    get_person as db_get_person,
    get_groups as db_get_groups,
    receive_timesheet as db_receive_timesheet,
    receive as db_receive,
    find_orgs as db_find_orgs,
    find_org as db_find_org,
)

from tools.cp1251 import decode_cp1251


async def send_timesheet_by_content(request: web.Request):
    content, filename = await _extract_content(request)
    lines = content.splitlines()
    if len(lines) < 2:
        return web.Response(text='Invalid file format', status=400)

    org_code, org_inn = lines[1].split(';')[:2]

    db = request.app['db']
    # Используем find_org, который принимает db
    org = await db_find_org(db, org_inn, org_code)  # или group, проверь, какой параметр нужен
    db_key = org['db_key']
    company_rn_str = org['company_rn'].to_str()
    logging.info('db_key: %s, company_rn: %s', db_key, company_rn_str)  # noqa: E501, E261, E231)

    if org:
        result = await db_send_timesheet(db, org['db_key'], org['company_rn'], content)
        status = 202
    else:
        result = f'Учреждение "{org_code}" с ИНН {org_inn} не найдено'
        status = 422

    return web.Response(text=f'{result}\n', status=status)


async def send_timesheet(request: web.Request):
    content, filename = await _extract_content2(request)

    data = await request.post()  # читает form-поля (не файлы)
    db_key = data.get('db_key')
    company_rn_str = data.get('company_rn')

    if not db_key or not company_rn_str:
        return web.Response(text='Missing db_key or company_rn', status=400)

    try:
        company_rn = int(company_rn_str)
    except ValueError:
        return web.Response(text='company_rn must be integer', status=400)

    db = request.app['db']
    result = await db_send_timesheet(db, db_key, company_rn, content)
    return web.Response(text=f'{result}\n')


async def get_orgs(request: web.Request):
    db = request.app['db']
    org_inn = request.rel_url.query.get('org_inn')
    if not org_inn:
        return web.Response(text='Missing org_inn', status=400)

    result = await db_find_orgs(db, org_inn)
    json_result = json.dumps(result, ensure_ascii=False)
    return web.Response(text=json_result, content_type='application/json')


async def get_person(request: web.Request):
    db = request.app['db']

    db_key = request.rel_url.query.get('db_key')
    org_rn_str = request.rel_url.query.get('org_rn')
    family = request.rel_url.query.get('family')
    firstname = request.rel_url.query.get('firstname')
    lastname = request.rel_url.query.get('lastname')

    if not all([db_key, org_rn_str, family, firstname, lastname]):
        return web.Response(text='Missing parameters: db_key, org_rn, family, firstname, lastname', status=400)

    try:
        org_rn = int(org_rn_str)
    except ValueError:
        return web.Response(text='org_rn must be integer', status=400)

    result = await db_get_person(db, db_key, org_rn, family, firstname, lastname)
    return web.Response(text=str(result))


async def get_groups(request: web.Request):
    db = request.app['db']

    db_key = request.rel_url.query.get('db_key')
    org_rn_str = request.rel_url.query.get('org_rn')

    if not db_key or not org_rn_str:
        return web.Response(text='Missing parameters: db_key, org_rn', status=400)

    try:
        org_rn = int(org_rn_str)
    except ValueError:
        return web.Response(text='org_rn must be integer', status=400)

    result = await db_get_groups(db, db_key, org_rn)
    return web.Response(text=str(result))


async def receive_timesheet(request: web.Request):
    db = request.app['db']

    db_key = request.rel_url.query.get('db_key')
    org_rn_str = request.rel_url.query.get('org_rn')
    group = request.rel_url.query.get('group')

    if not all([db_key, org_rn_str, group]):
        return web.Response(text='Missing parameters: db_key, org_rn, group', status=400)

    try:
        org_rn = int(org_rn_str)
    except ValueError:
        return web.Response(text='org_rn must be integer', status=400)

    content, filename = await db_receive_timesheet(db, db_key, org_rn, group)

    with aiohttp.MultipartWriter() as root:
        part = root.append(io.BytesIO(content))
        part.set_content_disposition('package', filename=filename)
        return web.Response(body=root)


async def receive(request: web.Request):
    db = request.app['db']

    org_inn = request.rel_url.query.get('org_inn')
    group = request.rel_url.query.get('group')

    if not org_inn or not group:
        return web.Response(text='Missing parameters: org_inn, group', status=400)

    try:
        content, filename = await db_receive(db, org_inn, group)

        with aiohttp.MultipartWriter() as root:
            part = root.append(io.BytesIO(content))
            part.set_content_disposition('package', filename=filename)
            return web.Response(body=root)
    except LookupError as e:
        return web.Response(status=412, text=str(e), charset='utf-8')


async def _extract_content(request: web.Request):
    post = await request.post()
    package = post.get('package')
    if not package:
        raise web.HTTPBadRequest(text='No file uploaded')

    encoded = package.file.read()
    content = decode_cp1251(encoded)
    return content, package.filename


async def _extract_content2(request: web.Request):
    async for part in (await request.multipart()):
        encoded = await part.read()
        content = decode_cp1251(encoded)
        filename = unquote_plus(part.filename)
        return content, filename
    return None
