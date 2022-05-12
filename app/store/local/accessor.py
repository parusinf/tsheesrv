from aiohttp import web
from sqlalchemy import Column, UniqueConstraint
from sqlalchemy import Integer
from sqlalchemy import String
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.ext.asyncio import create_async_engine
from sqlalchemy.orm import declarative_base
from sqlalchemy.orm import sessionmaker

Base = declarative_base()


class Org(Base):
    __tablename__ = 'org'
    id = Column(Integer, primary_key=True)
    org_code = Column(String, nullable=False)
    org_inn = Column(String, nullable=False)
    org_rn = Column(Integer, nullable=False)
    org_name = Column(String, nullable=False)
    company_rn = Column(Integer, nullable=False)
    company_name = Column(String, nullable=False)
    db_key = Column(String, nullable=False)
    __table_args__ = (UniqueConstraint('org_code', 'org_inn', name='_org_code_inn_uc'),)


class SqliteAccessor:
    def __init__(self) -> None:
        self.engine = None
        self.session = None

    def setup(self, app: web.Application):
        app.on_startup.append(self._on_connect)
        app.on_cleanup.append(self._on_disconnect)

    async def _on_connect(self, app: web.Application):
        self.engine = create_async_engine(
            f"sqlite+aiosqlite:///{app['config']['sqlite']['database']}?cache=shared",
            echo=app['config']['sqlite']['echo'],
        )
        async with self.engine.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)
        self.session = sessionmaker(
            self.engine, expire_on_commit=False, class_=AsyncSession
        )

    async def _on_disconnect(self, _):
        await self.engine.dispose()
