from aiohttp import web
import os
import logging
import cx_Oracle
import cx_Oracle_async


class OracleAccessor:
    def __init__(self) -> None:
        self.pool = {}

    def setup(self, app: web.Application):
        app.on_startup.append(self.on_connect)
        app.on_cleanup.append(self.on_disconnect)

    async def on_connect(self, app: web.Application):
        logging.info(f'Подключение пула баз данных')
        config = app['config']
        os.environ['NLS_LANG'] = config['oracle']['nls_lang']
        cx_Oracle.init_oracle_client(lib_dir=config['oracle']['lib_dir'])
        for db_key, db_param in config['database'].items():
            try:
                self.pool[db_key] = await cx_Oracle_async.create_pool(
                    host=db_param['host'],
                    port=db_param['port'],
                    service_name=db_param['service_name'],
                    user=db_param['user'],
                    password=db_param['password'],
                    min=config['oracle']['min_pool'],
                    max=config['oracle']['max_pool'],
                )
            except cx_Oracle.Error as error:
                logging.error(error)

    async def on_disconnect(self, _):
        logging.info(f'Отключение пула баз данных')
        for pool in self.pool.values():
            await pool.close()
