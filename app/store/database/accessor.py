from aiohttp import web
import os
import logging
import oracledb


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
                # Удаляем ключ, если что-то пошло не так, чтобы не было None
                if db_key in self.pool:
                    del self.pool[db_key]
                logging.exception(f'Ошибка создания пула {db_key}: {e}')

        logging.info(f'Всего активных пулов: {len(self.pool)}')

    async def on_disconnect(self, _):
        self.pool.clear()
        logging.info('Пулы очищены')
