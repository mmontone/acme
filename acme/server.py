import SocketServer
import logging
import msgpack
import configuration as conf
import json

class AcmeClientHandler(SocketServer.StreamRequestHandler):
    def send (self, data):
        #serialized_data = msgpack.packb(data)
        serialized_data = json.dumps(data)
        length = len(serialized_data)
        self.wfile.write('%d\n' % length)
        self.wfile.write(serialized_data)
        
    def handle (self):
        try:
            data = self.rfile.readline().strip()
            logging.info('Data received: ' + data)

            msg_type, msg_content = data.split(' ', 1)

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
            else:
                self.send({'error' : 'Invalid operation: ' + msg_type})
        except Exception as e:
            self.send({'error': str(e)})
            
class AcmeServer (SocketServer.ThreadingMixIn, SocketServer.TCPServer, object):
    "ACME server"
    
    def __init__(self, server_address):
        super(AcmeServer, self).__init__(server_address, AcmeClientHandler)

    def server_activate(self):
        logging.info('Server started: ' + str(self.server_address))
        SocketServer.TCPServer.server_activate(self)
        return

    # def serve_forever(self):
    #     logging.info('waiting for request')
    #     logging.info('Handling requests, press <Ctrl-C> to quit')
    #     while True:
    #         self.handle_request()
    #     return
