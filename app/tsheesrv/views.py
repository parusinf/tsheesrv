from aiohttp import web
from app.store.database.models import send_timesheet
from app.tsheesrv.models import extract_org_code_inn, find_org
from tools.cp1251 import decode_cp1251


class SendView(web.View):
    async def post(self):
        encoded = await self.request.content.read()
        content = decode_cp1251(encoded)
        org_code, org_inn = extract_org_code_inn(content)
        org = await find_org(org_code, org_inn)
        if org:
            result = await send_timesheet(org['db_key'], org['company_rn'], content)
            status = 202  # Accepted
        else:
            result = f'Учреждение "{org_code}" с ИНН {org_inn} не найдено'
            status = 422  # Unprocessable Entity
        return web.Response(text=f'{result}\n', status=status)
