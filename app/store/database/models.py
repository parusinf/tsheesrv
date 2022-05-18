from datetime import datetime
import cx_Oracle
import logging
from typing import Optional
from app.store.database.accessor import OracleAccessor
from tools.cp1251 import encode_cp1251
from tools.helpers import temp_filepath
import json

db = OracleAccessor()


async def get_orgs(db_key, org_inn) -> list[dict]:
    """
    Поиск списка учреждений по ИНН в текущей базе данных
    Для одного ИНН может быть одно или два учреждения: для основных групп и групп доп. образования
    """
    try:
        async with db.pool[db_key].acquire() as connection:
            async with connection.cursor() as cursor:
                orgs_json_var = await cursor.var(str)
                await cursor.callproc('UDO_P_GET_PSORGS', [org_inn, orgs_json_var])
                orgs_json = orgs_json_var.getvalue()
                if orgs_json:
                    orgs = json.loads(orgs_json)
                    for org in orgs:
                        org.update({'db_key': db_key})
                    return orgs
    except cx_Oracle.Error as error:
        logging.error(error)
    return []


async def find_orgs(org_inn) -> list[dict]:
    """Поиск Паруса, обслуживающего учреждения с заданным ИНН"""
    for db_key in db.pool.keys():
        orgs = await get_orgs(db_key, org_inn)
        if orgs:
            return orgs


async def get_person(db_key, org_rn, family, firstname, lastname) -> Optional[int]:
    """Поиск сотрудника в учреждении"""
    pool = db.pool[db_key]
    async with pool.acquire() as connection:
        async with connection.cursor() as cursor:
            person_rn_var = await cursor.var(int)
            await cursor.callproc('UDO_FIND_PERSON_BY_FIO', [org_rn, family, firstname, lastname, person_rn_var])
            return person_rn_var.getvalue()


async def get_groups(db_key, org_rn) -> Optional[str]:
    """Получение списка групп учреждения"""
    pool = db.pool[db_key]
    async with pool.acquire() as connection:
        async with connection.cursor() as cursor:
            groups_var = await cursor.var(str)
            await cursor.callproc('UDO_P_PSORG_GET_GROUPS', [org_rn, groups_var])
            return groups_var.getvalue()


async def receive_timesheet(db_key, org_rn, group, period=datetime.now()) -> tuple[bytes, str]:
    """Получение табеля посещаемости группы в файле CSV"""
    pool = db.pool[db_key]
    async with pool.acquire() as connection:
        async with connection.cursor() as cursor:
            filename_var = await cursor.var(str)
            content_var = await cursor.var(cx_Oracle.DB_TYPE_CLOB)
            await cursor.callproc('UDO_P_SEND_TIMESHEET', [org_rn, group, period, filename_var, content_var])
            content = content_var.getvalue().read()
            return encode_cp1251(content), filename_var.getvalue()


async def send_timesheet(db_key, company_rn, content) -> str:
    """Отправка табеля посещаемости группы в формате CSV в Парус"""
    pool = db.pool[db_key]
    async with pool.acquire() as connection:
        async with connection.cursor() as cursor:
            result_var = await cursor.var(str)
            await cursor.callproc('UDO_P_RECEIVE_TIMESHEET', [company_rn, content, result_var])
            await connection.commit()
            return result_var.getvalue()
