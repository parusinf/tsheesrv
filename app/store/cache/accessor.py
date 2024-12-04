import logging
from aiohttp import web
from sqlalchemy import Column, UniqueConstraint
from sqlalchemy import Integer
from sqlalchemy import String
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker
from sqlalchemy.ext.asyncio import create_async_engine
from sqlalchemy.orm import declarative_base

Base = declarative_base()


class Org(Base):
    __tablename__ = 'org'
    id = Column(Integer, primary_key=True)
    org_rn = Column(Integer, nullable=False)
    org_code = Column(String, nullable=False)
    org_name = Column(String, nullable=False)
    org_inn = Column(String, nullable=False)
    company_rn = Column(Integer, nullable=False)
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
        logging.info(f'Подключение кэша')
        self.engine = create_async_engine(
            f'sqlite+aiosqlite:///{app["config"]["sqlite"]["database"]}?cache=shared',
            echo=app['config']['sqlite']['echo'],
        )
        async with self.engine.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)
        self.session = async_sessionmaker(
            self.engine, class_=AsyncSession, expire_on_commit=False,
        )

    async def _on_disconnect(self, _):
        logging.info(f'Отключение кэша')
        await self.engine.dispose()
