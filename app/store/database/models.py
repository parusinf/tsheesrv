import oracledb
import logging
from typing import Optional, List, Tuple
from requests import JSONDecodeError
from tools.cp1251 import encode_cp1251
import os
import json
from datetime import datetime
from aiohttp import web

class OracleAccessor:
    def __init__(self) -> None:
        self.pool = {}

    def setup(self, app: web.Application):
        app.on_startup.append(self.on_connect)
        app.on_cleanup.append(self.on_disconnect)

    async def on_connect(self, app: web.Application):
        logging.info('Подключение пула баз данных (thin mode)')
        config = app['config']
        os.environ['NLS_LANG'] = config['oracle']['nls_lang']

        for db_key, db_param in config['database'].items():
            try:
                pool = oracledb.create_pool(
                    host=db_param['host'],
                    port=db_param['port'],
                    service_name=db_param['service_name'],
                    user=db_param['user'],
                    password=db_param['password'],
                    min=config['oracle']['min_pool'],
                    max=config['oracle']['max_pool'],
                    timeout=10,
                )
                self.pool[db_key] = pool
                logging.info(f'Пул {db_key} успешно создан (thin mode)')
            except Exception as e:
                if db_key in self.pool:
                    del self.pool[db_key]
                logging.exception(f'Ошибка создания пула {db_key}: {e}')

        logging.info(f'Всего активных пулов: {len(self.pool)}')

    async def on_disconnect(self, _):
        self.pool.clear()
        logging.info('Пулы очищены')


# --- Функции работы с данными (все принимают db первым аргументом!) ---

async def _with_conn(db, db_key: str, cb):
    """Вспомогательная функция для безопасного acquire/release"""
    if db_key not in db.pool:
        raise ValueError(f"Пул с ключом {db_key} не найден")

    pool = db.pool[db_key]
    conn = None
    try:
        conn = await pool.acquire()
        return await cb(conn)
    except oracledb.Error as e:
        logging.error("Oracle error (db_key=%s): %s", db_key, e)
        raise
    finally:
        if conn is not None:
            try:
                await pool.release(conn)
            except Exception:
                pass


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

# ... остальные функции (get_person, get_groups, receive и т.д.)
# тоже должны принимать 'db' первым аргументом и передавать его дальше ...
# Пример для receive:
async def receive(db, org_inn: str, group: str, period=datetime.now()) -> Tuple[bytes, str]:
    org = await find_org(db, org_inn, group)  # <-- передаем db
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
