from aiohttp import web
from app.settings import config


def setup_config(application):
    application['config'] = config


def setup_accessors(application):
    from app.store.database.models import db
    db.setup(application)
    from app.store.local.models import db as localdb
    localdb.setup(application)


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
