import logging
from aiohttp import web
from app.settings import config

logging.basicConfig(
    filename=config['log_file'] if config['use_log_file'] else None,
    level=logging.INFO)

def setup_config(application):
    application['config'] = config

def setup_accessors(application):
    # 1. Импортируем КЛАСС, а не переменную
    from app.store.database.models import OracleAccessor

    # 2. Создаём экземпляр
    db = OracleAccessor()

    # 3. Настраиваем (вешаем on_startup/on_cleanup)
    db.setup(application)

    # 4. Сохраняем экземпляр в приложение, чтобы хендлеры могли его взять
    application['db'] = db

    # Для кэша делаем аналогично, если localdb тоже был классом
    # from app.store.cache.models import CacheAccessor
    # cache_db = CacheAccessor()
    # cache_db.setup(application)

def setup_routes(application):
    from app.tsheesrv.routes import setup_routes as setup_tsheesrv_routes
    setup_tsheesrv_routes(application)

def setup_app(application):
    setup_config(application)
    setup_accessors(application)
    setup_routes(application)

app = web.Application()

if __name__ == '__main__':
    setup_app(app)
    web.run_app(app, host=config['webapp']['host'], port=config['webapp']['port'])
