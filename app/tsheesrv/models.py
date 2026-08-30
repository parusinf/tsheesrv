from datetime import datetime
import oracledb
import logging
from typing import Optional, List, Tuple
from requests import JSONDecodeError
import json
from tools.cp1251 import encode_cp1251



async def _with_conn(db, db_key: str, cb):
    """
    cb — async callable, принимающий connection.
    acquire/release — синхронные в oracledb.
    """
    pool = db.pool.get(db_key)
    if not pool:
        raise ValueError(f"Пул с ключом {db_key} не найден. Доступные ключи: {list(db.pool.keys())}")

    conn = None
    try:
        # acquire — синхронный, НЕ делаем await
        conn = pool.acquire()
        return await cb(conn)
    except oracledb.Error as e:
        logging.error("Oracle error (db_key=%s): %s", db_key, e)
        raise
    finally:
        if conn is not None:
            try:
                # release — тоже синхронный
                pool.release(conn)
            except Exception:
                pass


# --- Функции работы с данными ---

async def get_orgs(db, db_key: str, org_inn: str) -> List[dict]:
    async def body(conn):
        async with conn.cursor() as cursor:
            orgs_json_var = await cursor.var(str)
            await cursor.callproc('UDO_P_GET_PSORGS', [org_inn, orgs_json_var])
            orgs_json = orgs_json_var.getvalue()
            if orgs_json:
                orgs = json.loads(orgs_json)
                for org in orgs:
                    org.update({'db_key': db_key})
                return orgs
            return []
    return await _with_conn(db, db_key, body)


async def get_org(db, db_key: str, org_inn: str, group: str) -> Optional[dict]:
    async def body(conn):
        async with conn.cursor() as cursor:
            org_json_var = await cursor.var(str)
            await cursor.callproc('UDO_P_GET_PSORG', [org_inn, group, org_json_var])
            org_json = org_json_var.getvalue()
            if org_json:
                org = json.loads(org_json)
                org.update({'db_key': db_key})
                return org
            return None

    try:
        return await _with_conn(db, db_key, body)
    except oracledb.Error:
        return None
    except JSONDecodeError:
        logging.error("JSON decode error in get_org")
        return None


async def find_orgs(db, org_inn: str) -> List[dict]:
    for db_key in db.pool.keys():
        orgs = await get_orgs(db, db_key, org_inn)
        if orgs:
            return orgs
    return []


async def find_org(db, org_inn: str, group: str) -> Optional[dict]:
    for db_key in db.pool.keys():
        org = await get_org(db, db_key, org_inn, group)
        if org:
            return org
    return None


async def get_person(db, db_key: str, org_rn: int, family: str, firstname: str, lastname: str) -> Optional[int]:
    async def body(conn):
        async with conn.cursor() as cursor:
            person_rn_var = await cursor.var(int)
            await cursor.callproc(
                'UDO_FIND_PERSON_BY_FIO',
                [org_rn, family, firstname, lastname, person_rn_var]
            )
            return person_rn_var.getvalue()
    try:
        return await _with_conn(db, db_key, body)
    except oracledb.Error:
        return None


async def get_groups(db, db_key: str, org_rn: int) -> Optional[str]:
    async def body(conn):
        async with conn.cursor() as cursor:
            groups_var = await cursor.var(str)
            await cursor.callproc('UDO_P_PSORG_GET_GROUPS', [org_rn, groups_var])
            return groups_var.getvalue()
    try:
        return await _with_conn(db, db_key, body)
    except oracledb.Error:
        return None


async def receive_timesheet(db, db_key: str, org_rn: int, group: str, period=datetime.now()) -> Tuple[bytes, str]:
    async def body(conn):
        async with conn.cursor() as cursor:
            filename_var = await cursor.var(str)
            content_var = await cursor.var(oracledb.DB_TYPE.CLOB)
            await cursor.callproc(
                'UDO_P_TIMESHEET_SEND',
                [org_rn, group, period, filename_var, content_var]
            )
            content = content_var.getvalue().read()
            return encode_cp1251(content), filename_var.getvalue()
    return await _with_conn(db, db_key, body)


async def receive(db, org_inn: str, group: str, period=datetime.now()) -> Tuple[bytes, str]:
    org = await find_org(db, org_inn, group)
    if org is None:
        raise LookupError(f'В учреждении с ИНН {org_inn} группа с мнемокодом "{group}" не найдена')

    async def body(conn):
        async with conn.cursor() as cursor:
            filename_var = await cursor.var(str)
            content_var = await cursor.var(oracledb.DB_TYPE.CLOB)
            await cursor.callproc(
                'UDO_P_TIMESHEET_SEND',
                [org['org_rn'], group, period, filename_var, content_var]
            )
            content = content_var.getvalue().read()
            return encode_cp1251(content), filename_var.getvalue()
    return await _with_conn(db, org['db_key'], body)


async def send_timesheet(db, db_key: str, company_rn: int, content: str) -> str:
    async def body(conn):
        async with conn.cursor() as cursor:
            result_var = await cursor.var(str)
            await cursor.callproc('UDO_P_TIMESHEET_RECEIVE', [company_rn, content, result_var])
            await conn.commit()
            return result_var.getvalue()
    return await _with_conn(db, db_key, body)
