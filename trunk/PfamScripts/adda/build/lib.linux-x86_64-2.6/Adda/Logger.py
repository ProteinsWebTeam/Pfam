import multiprocessing, logging, sys, re, os, StringIO, threading, time, Queue, collections

from logging import Logger

class MultiProcessingLogHandler(logging.Handler):
    """taken from http://stackoverflow.com/questions/641420/how-should-i-log-while-using-multiprocessing-in-python

    added counting of log messages.
    """
    def __init__(self, handler, queue, child=False):
        logging.Handler.__init__(self)

        self._handler = handler
        self.queue = queue

        self.counts = collections.defaultdict( int )

        # we only want one of the loggers to be pulling from the queue.
        # If there is a way to do this without needing to be passed this
        # information, that would be great!
        if child == False:
            self.shutdown = False
            self.polltime = 1
            t = threading.Thread(target=self.receive)
            t.daemon = True
            t.start()

    def setFormatter(self, fmt):
        logging.Handler.setFormatter(self, fmt)
        self._handler.setFormatter(fmt)

    def receive(self):
        while (self.shutdown == False) or (self.queue.empty() == False):
            # so we block for a short period of time so that we can
            # check for the shutdown cases.
            try:
                record = self.queue.get(True, self.polltime)
                self._handler.emit(record)
                self.counts[record.levelname] += 1
            except Queue.Empty, e:
                pass
            except EOFError, e:
                break

    def send(self, s):
        # send just puts it in the queue for the server to retrieve
        self.queue.put(s)

    def _format_record(self, record):
        ei = record.exc_info
        if ei:
            dummy = self.format(record) # just to get traceback text into record.exc_text
            record.exc_info = None  # to avoid Unpickleable error

        return record

    def emit(self, record):
        try:
            s = self._format_record(record)
            self.send(s)
        except (KeyboardInterrupt, SystemExit):
            raise
        except:
            self.handleError(record)

    def close(self):
        time.sleep(self.polltime+1) # give some time for messages to enter the queue.
        self.shutdown = True
        time.sleep(self.polltime+1) # give some time for the server to time out and see the shutdown

    def __del__(self):
        self.close() # hopefully this aids in orderly shutdown when things are going poorly.

    def getCounts( self ):
        return self.counts

def f(x):
    # just a logging command...
    logging.critical('function number: ' + str(x))
    # to make some calls take longer than others, so the output is "jumbled" as real MP programs are.
    time.sleep(x % 3)

def initPool(queue, level):
    """
    This causes the logging module to be initialized with the necessary info
    in pool threads to work correctly.
    """
    logging.getLogger('').addHandler(MultiProcessingLogHandler(logging.StreamHandler(), queue, child=True))
    logging.getLogger('').setLevel(level)

