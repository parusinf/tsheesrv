from typing import Optional

import app.store.database.models as database
import app.store.cache.models as cache


async def get_orgs(org_inn) -> list[dict]:
    """Поиск учреждений по ИНН в кэше либо в базах данных с кэшированием"""
    # Поиск учреждений по ИНН в кэше
    orgs = await cache.get_orgs(org_inn)
    # В кэше нет учреждений с таким ИНН
    if len(orgs) == 0:
        # Поиск базы данных и учреждений по ИНН
        orgs = await database.find_orgs(org_inn)
        # Кэширование учреждений
        await cache.insert_orgs(orgs)
    # В кеше одно учреждение с таким ИНН
    elif len(orgs) == 1:
        # Поиск учреждений по ключу базы данных из кеша и ИНН на случай добавления учреждения в базе данных
        orgs = await database.get_orgs(orgs[0]['db_key'], org_inn)
        # Кеширование нового учреждения (существующее учреждение добавлено не будет)
        if len(orgs) == 2:
            await cache.insert_orgs(orgs)
    return orgs


async def get_org(org_code, org_inn) -> Optional[dict]:
    """Поиск учреждения по мнемокоду и ИНН в кэше либо в базах данных по ИНН с кэшированием"""
    # Поиск учреждения по мнемокоду и ИНН в кэше
    org = await cache.get_org(org_code, org_inn)
    # В кэше нет учреждения с таким мнемокодом и ИНН
    if not org:
        # Поиск учреждений по ИНН в кэше либо в базах данных с кэшированием
        orgs = await get_orgs(org_inn)
        # Поиск учреждения по мнемокоду и ИНН
        for o in orgs:
            if org_code == o['org_code'] and org_inn == o['org_inn']:
                org = o
                break
    return org
