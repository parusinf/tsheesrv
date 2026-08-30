import oracledb
import logging

class OracleAccessor:
    """
    Только управляет пулами. Не выполняет запросы.
    Экземпляр создаётся в main.py и кладётся в app['db'].
    """
    def __init__(self) -> None:
        self.pool = {}

    def setup(self, app) -> None:
        app.on_startup.append(self.on_connect)
        app.on_cleanup.append(self.on_disconnect)

    async def on_connect(self, app) -> None:
        logging.info('Подключение пулов баз данных (thin mode)')
        config = app['config']
        import os
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
                if db_key in self.pool:
                    del self.pool[db_key]
                logging.exception(f'Ошибка создания пула {db_key}: {e}')

        logging.info(f'ИТОГО пулов в on_connect: {len(self.pool)}')

    async def on_disconnect(self, _) -> None:
        self.pool.clear()
        logging.info('Пулы очищены')
