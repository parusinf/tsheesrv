from typing import Optional, List

from sqlalchemy import insert, ColumnElement
from sqlalchemy.future import select
from app.store.cache.accessor import SqliteAccessor, Org
from app.store.cache.tools import row_to_dict, rows_to_list

db = SqliteAccessor()


async def get_orgs(org_inn) -> List[dict]:
    async with db.session() as session:
        stmt = select(Org).where(ColumnElement[org_inn == Org.org_inn])
        result = await session.execute(stmt)
        return rows_to_list(result)


async def get_org(org_code, org_inn) -> Optional[dict]:
    async with db.session() as session:
        stmt = select(Org).where(
            ColumnElement[org_inn == Org.org_inn],
            ColumnElement[org_code == Org.org_code])
        result = await session.execute(stmt)
        return row_to_dict(result.first())


async def insert_org(org):
    async with db.session() as session:
        async with session.begin():
            stmt = insert(Org).values(**org)
            await session.execute(stmt)
        await session.commit()


async def insert_orgs(orgs):
    for o in orgs:
        await insert_org(o)
