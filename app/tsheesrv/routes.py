from app.tsheesrv import views


def setup_routes(app):
    token = app['config']['token']
    app.router.add_view(f'/{token}', views.SendView)
