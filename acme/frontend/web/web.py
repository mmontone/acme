from bottle import route, run, template, static_file

def edit_configurations(host='localhost', port='8080'):
    "Edit configurations via web interface"
    run(host=host, port=port)

@route('/edit')
def edit_configurations_handler():
    "Edit configurations handler"
    return '<b>Hello</b>!'

@route('/static/<path:path>')
def static_handler(path):
    "Static files handler"
    return static_file(path, root='static')

edit_configurations()


