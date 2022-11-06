from app.tsheesrv import views


def setup_routes(app):
    token = app['config']['web_token']
    app.router.add_post(f'/{token}', views.send_timesheet_by_content)
    app.router.add_post(f'/{token}/send_timesheet', views.send_timesheet)
    app.router.add_get(f'/{token}/get_orgs', views.get_orgs)
    app.router.add_get(f'/{token}/get_person', views.get_person)
    app.router.add_get(f'/{token}/get_groups', views.get_groups)
    app.router.add_get(f'/{token}/receive_timesheet', views.receive_timesheet)
    app.router.add_get(f'/{token}/receive', views.receive)
