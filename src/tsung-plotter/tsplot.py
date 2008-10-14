#! /usr/bin/python
# -*- coding: utf-8 -*-

#
#  Created: 2006 by Dimitri Fontaine <dim@tapoueh.org>
#
#  Modified by Nicolas Niclausse
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
#
#  In addition, as a special exception, you have the permission to
#  link the code of this program with any library released under
#  the EPL license and distribute linked combinations including
#  the two.

# A plotter for tsung text generated data

from tsung.tsung import TsungLog
import os, sys
from ConfigParser import ConfigParser
from pylab import *

SYSDIR         = "./"
USERDIR        = ".tsung"
CONF           = "plots.conf"
SYS_STATS_CONF = os.path.join(SYSDIR, 'tsung', 'stats.conf')

class Plot:
    """ The ploting class, using matplotlib """

    def __init__(self,
                 name    = "",
                 xlabel  = "",
                 ylabel  = "",
                 title   = "",
                 legends = None,
                 styles  = ['b-', 'r-', 'g.', 'c,', 'y^', 'kv'],
                 dpi     = 150,
                 tn_dpi  = 50,
                 xfactor = 1,
                 yfactor = 1,
                 outdir  = '/tmp/tsung-plotter',
                 imgtype = 'png'):

        # Only used if no plot type given
        self.name      = name
        self.xlabel    = xlabel
        self.ylabel    = ylabel
        self.title     = title
        self.legends   = legends
        self.dpi       = dpi
        self.thumb_dpi = tn_dpi
        self.xfactor   = xfactor
        self.yfactor   = yfactor
        self.styles    = styles
        self.outdir    = outdir
        self.imgtype   = imgtype

    def plot(self, stats, dataset):
        """ draw a simple timeline plots from two dataset """

        # prepare matplotlib graph
        clf()
        ax = subplot(111)

        count = 0
        for data in dataset:
            # get give type data for plotting
            for (name, stat) in stats:
                pdata = data.stat(name, stat)

                # we want timestamp sorted data to plot
                ts = pdata.keys()
                ts.sort()
                values = [pdata[t] / self.yfactor for t in ts]

                if self.xfactor not in [1, "1"]:
                    ts = [x / self.xfactor for x in ts]

                # Now use matplotlib to do the plotting
                plot(ts, values, self.styles[count%len(self.styles)])
                count += 1

        # Now setup the legend
        title(self.title)
        xlabel(self.xlabel)
        ylabel(self.ylabel)
        legend(self.legends)

        # we want to draw a grid
        grid(True)

        filename = os.path.join(self.outdir,
                                "%s.%s" % (self.name, self.imgtype))
        thumbnail = os.path.join(self.outdir,
                                 "%s_tn.%s" % (self.name, self.imgtype))

        savefig(filename, dpi=self.dpi, orientation='portrait')
        savefig(thumbnail, dpi=self.thumb_dpi, orientation='portrait')

        return filename


def main(conffile, logs, legends, outdir, verbose):
    """ Produce plots from given """

    dataset = [d for f, d in logs]

    config = ConfigParser()
    config.read(conffile)

    for s in config.sections():
        p = Plot(name = s, outdir = outdir)

        # defaults
        for d, v in config.defaults().items():
            if d != 'encoding':
                p.__dict__[d] = v
            else:
                encoding = v

        # stats
        # conf:   200.count, 400.count
        # result: [["200", "count"], ["400", "count"]]
        try:
            stats = [x.strip().split('.')
                     for x in config.get(s, 'stats').split(' ')]
        except:
            print 'error: unable to read plot "%s" stats' % s
            continue

        if config.has_option(s, 'styles'):
            p.styles = [x.strip() for x in config.get(s, 'styles').split(' ')]

        if config.has_option(s, 'legend'):
            # this is the legend prefix$
            l = []
            count    = 0
            clegends = config.get(s, 'legend').decode(encoding).split(',')

            for f in logs:
                l     += ['%s %s' % (x.strip(), legends[count]) \
                          for x in clegends]
                count += 1

        p.legends = l

        # Text parameters - to decode into specified encoding
        for attr in ['title', 'xlabel', 'ylabel']:
            if config.has_option(s, attr):
                cstring = config.get(s, attr).decode(encoding)
                p.__dict__[attr] = cstring

        # Numerical parameters
        for attr in ['xfactor', 'yfactor', 'dpi', 'tn_dpi']:
            if config.has_option(s, attr):
                try:
                    p.__dict__[attr] = config.getfloat(s, attr)
                except ValueError:
                    print 'warning: %s %s not a number: %s' \
                          % (p.name, attr, config.get(s, attr))

        outfile = p.plot(stats, dataset)

        if verbose:
            print 'Generated plot %s' % outfile

if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-c", "--config", dest="config", default=None,
                      help="configuration file")
    parser.add_option("-d", "--outdir", dest="outdir", default="/tmp/tsung",
                      help="output dir where to save plots (/tmp/tsung)")
    parser.add_option("-v", action="store_true", dest="verbose", default=False,
                      help="be verbose")

    (options, args) = parser.parse_args()

    if options.config is None:
        userconf = os.path.join(os.environ['HOME'], USERDIR, CONF)
        if os.access(userconf, os.R_OK):
            config = userconf
        else:
            config = os.path.join(SYSDIR, CONF)
    else:
        config = options.config

    if options.verbose:
        print 'Using %s configuration file' % config

    if not os.access(config, os.R_OK):
        print "can't read configuration file: %s" % config
        sys.exit(1)

    # FIXME: error control
    # OSError: [Errno 17] Le fichier existe.: '/tmp/tsung'
    try:
        os.makedirs(options.outdir)
    except:
        pass

    # args are legend then file, any times wanted by user
    if len(args) % 2 != 0:
        print "error: please provide legend and tsung log filename"
        sys.exit(3)

    count   = 0
    legends = []
    files   = []

    for a in args:
        if count % 2 == 0:
            legends.append(a)
        else:
            files.append(a)

        count += 1

    if options.verbose:
        print 'Using %s stats configuration file' % SYS_STATS_CONF

    logs = []
    for logfile in files:
        if not os.access(logfile, os.R_OK):
            print "error: unable to read file %s" % logfile

        else:
            if options.verbose:
                print 'Parsing Tsung log file', logfile
            logs.append((logfile, TsungLog(SYS_STATS_CONF, logfile)))

    if len(logs) != len(args) / 2:
        print 'error while parsing files (%d != %d)' % (len(logs),
                                                        len(args)/2)
        sys.exit(2)

    main(config, logs, legends, options.outdir, options.verbose)
