import app.store.database.models as database
import app.store.local.models as local


def extract_org_code_inn(content) -> list[str]:
    return content.splitlines()[1].split(';')[:2]


async def find_org(org_code, org_inn):
    org = await local.find_org(org_code, org_inn)
    if not org:
        orgs = await database.find_orgs(org_inn)
        if orgs:
            for o in orgs:
                if org_code == o['org_code'] and org_inn == o['org_inn']:
                    org = o
                    await local.insert_org(o)
                else:
                    if not (await local.find_org(o['org_code'], o['org_inn'])):
                        await local.insert_org(o)
    return org
