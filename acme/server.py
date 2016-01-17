import SocketServer
import logging
import msgpack
import configuration as conf
import json
import acme
import traceback

class AcmeClientHandler(SocketServer.StreamRequestHandler, object):
    def __init__(self, request, client_address, server):
        self.schemas = acme.load_schemas(server.args)
        self.configs = acme.load_configs(server.args)
                
        print self.configs
        print self.schemas
        
        super(AcmeClientHandler, self).__init__(request, client_address, server)
        
    def send (self, data):
        #serialized_data = msgpack.packb(data)
        serialized_data = json.dumps(data)
        length = len(serialized_data)
        self.wfile.write('%d\n' % length)
        self.wfile.write(serialized_data)
        
    def handle (self):
        while True:
            try:
                data = self.rfile.readline().strip()
                logging.info('Data received: ' + data)

                msg = data.split(' ', 1)
                msg_type = msg[0]
                
                msg_content = msg[1] if len(msg) > 1 else None
                
                if msg_type == 'GET':
                    split_path = msg_content.split('.')
                    config_name = split_path[0]
                    option_path = split_path[1:]

                    logging.info('Trying to read option ' + str(option_path) + ' from ' + config_name + ' configuration')
                    config = conf.Configuration.get_named(config_name)
                    option = config.schema.option_in_path(option_path)
                    value, origin = config.option_value(option)
                    logging.info(str(option_path) + ' option value: ' + str(value))

                    option_value = value
                    if option_value is None:
                        option_value = option.default_value

                    if option_value is None:
                        self.send('OK')
                    else:
                        option_origin = origin

                    if option_origin is None:
                        option_origin = 'Default'

                    attributes = {'value' : option_value,
                                  'type': str(option.option_type),
                                  'origin' : str(option_origin)}
                    self.send(attributes)

                elif msg_type == 'SET':
                    full_option_path, value = msg_content.split('=', 1)

                    logging.info('Trying to set option ' + str(option_path) + ' from ' + config_name + ' configuration')

                    split_path = full_option_path.split('.')
                    config_name = split_path[0]
                    option_path = split_path[1:]

                    config = conf.Configuration.get_named(config_name)
                    option = config.schema.option_in_path(option_path)
                    parsed_value = option.parse_value(value)

                    config.set_option_value(option, parsed_value)
                    self.send('OK')
                elif msg_type == 'RELOAD':
                    pass
                elif msg_type == 'LIST-SCHEMAS':
                    pass
                elif msg_type == 'LIST-CONFIGS':
                    self.send(map(lambda c: c.name, self.configs))
                else:
                    self.send({'error' : 'Invalid operation: ' + msg_type})
            except Exception as e:
                self.send({'error': str(e), 'backtrace': traceback.format_exc()})
            
class AcmeServer (SocketServer.ThreadingMixIn, SocketServer.TCPServer, object):
    "ACME server"
    
    def __init__(self, server_address, args):
        self.args = args
        super(AcmeServer, self).__init__(server_address, AcmeClientHandler)

    def server_activate(self):
        logging.info('Server started: ' + str(self.server_address))
        SocketServer.TCPServer.server_activate(self)
        return

    def serve_forever(self):
        logging.info('waiting for request')
        logging.info('Handling requests, press <Ctrl-C> to quit')
        while True:
            self.handle_request()
        return
