import io
import json
import aiohttp
from aiohttp import web
from urllib.parse import unquote_plus
import app.store.database.models as database
import app.tsheesrv.models as tsheesrv
from tools.cp1251 import decode_cp1251


async def send_timesheet_by_content(request: web.Request):
    content, filename = await _extract_content(request)
    lines = content.splitlines()
    org_code, org_inn = lines[1].split(';')[:2]
    org = await tsheesrv.get_org(org_code, org_inn)
    if org:
        result = await database.send_timesheet(org['db_key'], org['company_rn'], content)
        status = 202  # Accepted
    else:
        result = f'Учреждение "{org_code}" с ИНН {org_inn} не найдено'
        status = 422  # Unprocessable Entity
    return web.Response(text=f'{result}\n', status=status)


async def send_timesheet(request: web.Request):
    content, filename = await _extract_content2(request)
    db_key = request.rel_url.query['db_key']
    company_rn = request.rel_url.query['company_rn']
    result = await database.send_timesheet(db_key, company_rn, content)
    return web.Response(text=f'{result}\n')


async def get_orgs(request: web.Request):
    result = await tsheesrv.get_orgs(**request.rel_url.query)
    json_result = json.dumps(result)
    return web.Response(text=json_result)


async def get_person(request: web.Request):
    result = await database.get_person(**request.rel_url.query)
    return web.Response(text=str(result))


async def get_groups(request: web.Request):
    result = await database.get_groups(**request.rel_url.query)
    return web.Response(text=str(result))


async def receive_timesheet(request: web.Request):
    content, filename = await database.receive_timesheet(**request.rel_url.query)
    with aiohttp.MultipartWriter() as root:
        part = root.append(io.BytesIO(content))
        part.set_content_disposition('package', filename=filename)
        return web.Response(body=root)


async def _extract_content(request: web.Request):
    post = await request.post()
    package = post.get('package')
    encoded = package.file.read()
    content = decode_cp1251(encoded)
    return content, package.filename


async def _extract_content2(request: web.Request):
    async for part in (await request.multipart()):
        encoded = await part.read()
        content = decode_cp1251(encoded)
        filename = unquote_plus(part.filename)
        return content, filename
