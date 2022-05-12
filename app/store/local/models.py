from app.store.local.accessor import SqliteAccessor, Org
from sqlalchemy.future import select

db = SqliteAccessor()


def row_to_dict(row):
    if row:
        (obj,) = row
        out = dict(obj.__dict__)
        del out['_sa_instance_state']
        return out
    else:
        return None


async def find_org(org_code, org_inn):
    async with db.session() as session:
        query = select(Org).where(org_code == Org.org_code and org_inn == Org.org_inn)
        results = await session.execute(query)
        return row_to_dict(results.first())


async def insert_org(org):
    async with db.session() as session:
        async with session.begin():
            session.add(Org(
                org_code=org['org_code'],
                org_inn=org['org_inn'],
                org_rn=org['org_rn'],
                org_name=org['org_name'],
                company_rn=org['company_rn'],
                company_name=org['company_name'],
                db_key=org['db_key'],
            ))
        await session.commit()

