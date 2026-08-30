from datetime import datetime
import oracledb
import logging
from typing import Optional, List, Tuple
from requests import JSONDecodeError
from app.store.database.accessor import OracleAccessor
from tools.cp1251 import encode_cp1251
import json

db = OracleAccessor()


async def get_orgs(db_key: str, org_inn: str) -> List[dict]:
    """
    Поиск списка учреждений по ИНН в заданной базе данных.
    """
    conn = None
    try:
        # ✅ acquire() — это корутина, её обязательно await
        conn = await db.pool[db_key].acquire()
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
    except oracledb.Error as error:
        logging.error(error)
        return []
    except JSONDecodeError:
        logging.error('Ошибка разбора JSON: значение не получено или некорректно')
        return []
    finally:
        # ✅ release() тоже корутина
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                # Игнорируем ошибки при release, чтобы не ломать основной ответ
                pass


async def get_org(db_key: str, org_inn: str, group: str) -> Optional[dict]:
    """
    Поиск учреждения по ИНН и мнемокоду группы.
    """
    conn = None
    try:
        conn = await db.pool[db_key].acquire()
        async with conn.cursor() as cursor:
            org_json_var = await cursor.var(str)
            await cursor.callproc('UDO_P_GET_PSORG', [org_inn, group, org_json_var])
            org_json = org_json_var.getvalue()
            if org_json:
                org = json.loads(org_json)
                org.update({'db_key': db_key})
                return org
            return None
    except oracledb.Error as error:
        logging.error(error)
        return None
    except JSONDecodeError:
        logging.error('Ошибка разбора JSON')
        return None
    finally:
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                pass


async def find_orgs(org_inn: str) -> List[dict]:
    for db_key in db.pool.keys():
        orgs = await get_orgs(db_key, org_inn)
        if orgs:
            return orgs
    return []


async def find_org(org_inn: str, group: str) -> Optional[dict]:
    for db_key in db.pool.keys():
        org = await get_org(db_key, org_inn, group)
        if org:
            return org
    return None


async def get_person(db_key: str, org_rn: int, family: str, firstname: str, lastname: str) -> Optional[int]:
    conn = None
    try:
        conn = await db.pool[db_key].acquire()
        async with conn.cursor() as cursor:
            person_rn_var = await cursor.var(int)
            await cursor.callproc('UDO_FIND_PERSON_BY_FIO', [org_rn, family, firstname, lastname, person_rn_var])
            return person_rn_var.getvalue()
    except oracledb.Error as e:
        logging.error(e)
        return None
    finally:
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                pass


async def get_groups(db_key: str, org_rn: int) -> Optional[str]:
    conn = None
    try:
        conn = await db.pool[db_key].acquire()
        async with conn.cursor() as cursor:
            groups_var = await cursor.var(str)
            await cursor.callproc('UDO_P_PSORG_GET_GROUPS', [org_rn, groups_var])
            return groups_var.getvalue()
    except oracledb.Error as e:
        logging.error(e)
        return None
    finally:
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                pass


async def receive_timesheet(db_key: str, org_rn: int, group: str, period=datetime.now()) -> Tuple[bytes, str]:
    conn = None
    try:
        conn = await db.pool[db_key].acquire()
        async with conn.cursor() as cursor:
            filename_var = await cursor.var(str)
            content_var = await cursor.var(oracledb.DB_TYPE.CLOB)
            await cursor.callproc('UDO_P_TIMESHEET_SEND', [org_rn, group, period, filename_var, content_var])
            content = content_var.getvalue().read()
            return encode_cp1251(content), filename_var.getvalue()
    except oracledb.Error as e:
        logging.error(e)
        raise
    finally:
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                pass


async def receive(org_inn: str, group: str, period=datetime.now()) -> Tuple[bytes, str]:
    org = await find_org(org_inn, group)
    if org is None:
        raise LookupError(f'В учреждении с ИНН {org_inn} группа с мнемокодом "{group}" не найдена')

    conn = None
    try:
        conn = await db.pool[org['db_key']].acquire()
        async with conn.cursor() as cursor:
            filename_var = await cursor.var(str)
            content_var = await cursor.var(oracledb.DB_TYPE.CLOB)
            await cursor.callproc('UDO_P_TIMESHEET_SEND', [org['org_rn'], group, period, filename_var, content_var])
            content = content_var.getvalue().read()
            return encode_cp1251(content), filename_var.getvalue()
    except oracledb.Error as e:
        logging.error(e)
        raise
    finally:
        if conn is not None:
            try:
                await db.pool[org['db_key']].release(conn)
            except Exception:
                pass


async def send_timesheet(db_key: str, company_rn: int, content: str) -> str:
    conn = None
    try:
        conn = await db.pool[db_key].acquire()
        async with conn.cursor() as cursor:
            result_var = await cursor.var(str)
            await cursor.callproc('UDO_P_TIMESHEET_RECEIVE', [company_rn, content, result_var])
            await conn.commit()
            return result_var.getvalue()
    except oracledb.Error as e:
        logging.error(e)
        raise
    finally:
        if conn is not None:
            try:
                await db.pool[db_key].release(conn)
            except Exception:
                pass
