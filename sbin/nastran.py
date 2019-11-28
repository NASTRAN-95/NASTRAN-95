#!/usr/bin/python
################################################################################
# NASTRAN BOOTSTRAP SCRIPT
# D. Everhart
# 10 FEB 2016
################################################################################
# The MIT License (MIT)
# 
# Copyright (c) 2016 Daniel Everhart
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the 
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
#################################################################################  Version 
__version_major__  = 0
__version_minor__  = 0
__version_bugfix__ = 0
__version_info__   = (str(__version_major__),
                      str(__version_minor__),
                      str(__version_bugfix__))
__version__        = '.'.join(__version_info__)
################################################################################
import argparse
import sys
import os.path
import uuid

def main(args):
  workdir = os.path.abspath(os.curdir)
  nastran_home=os.path.abspath(os.path.dirname(sys.argv[0]) + '/..')
  nastran_x = '{0}/bin/nastran.x'.format(nastran_home)
  rstr = str(uuid.uuid4()).split('-')[0]

  bname = ''
  if os.path.isfile(args.input):
    bname = os.path.splitext(os.path.split(args.input)[1])[0]
  if len(bname) < 1: raise ValueError('Valid input file not provided.')

  if len(args.output_dir) > 0:
    outputdir = os.path.abspath(args.output_dir)
  else:
    outputdir = workdir

  tmpdir = os.environ.get('TEMP')
  if tmpdir is None: tmpdir = os.environ.get('TMP')
  if tmpdir is None: tmpdir = workdir
  DIRCTY  = os.path.join(tmpdir,rstr)
  RFDIR   = os.path.join(nastran_home,'rf')
  NPTPNM  = os.path.join(outputdir, '{0}.nptp' .format(bname))
  PLTNM   = os.path.join(outputdir, '{0}.plt'  .format(bname))
  DICTNM  = os.path.join(outputdir, '{0}.dict' .format(bname))
  PUNCHNM = os.path.join(outputdir, '{0}.pch'  .format(bname))
  OPTPNM  = os.path.join(outputdir, '{0}.opt'  .format(bname))
  LOGNM   = os.path.join(outputdir, '{0}.f04'  .format(bname))
  F06     = os.path.join(outputdir, '{0}.f06'  .format(bname))
                                                              
  IN12    = os.path.join(outputdir, '{0}.in12' .format(bname))
  OUT11   = os.path.join(outputdir, '{0}.out11'.format(bname))
                                                              
  FTN11   = os.path.join(outputdir, '{0}.f11'  .format(bname))
  FTN12   = os.path.join(outputdir, '{0}.f12'  .format(bname))
  FTN13   = os.path.join(outputdir, '{0}.f13'  .format(bname))
  FTN14   = os.path.join(outputdir, '{0}.f14'  .format(bname))
  FTN15   = os.path.join(outputdir, '{0}.f15'  .format(bname))
  FTN16   = os.path.join(outputdir, '{0}.f16'  .format(bname))
  FTN17   = os.path.join(outputdir, '{0}.f17'  .format(bname))
  FTN18   = os.path.join(outputdir, '{0}.f18'  .format(bname))
  FTN19   = os.path.join(outputdir, '{0}.f19'  .format(bname))
  FTN20   = os.path.join(outputdir, '{0}.f20'  .format(bname))
  FTN21   = os.path.join(outputdir, '{0}.f21'  .format(bname))
  FTN22   = os.path.join(outputdir, '{0}.f22'  .format(bname))
  FTN23   = os.path.join(outputdir, '{0}.f23'  .format(bname))
                                                              
  SOF1    = os.path.join(outputdir, '{0}.sof1' .format(bname))
  SOF2    = os.path.join(outputdir, '{0}.sof2' .format(bname))

  if len(args.OPTPNM) > 0: OPTPNM = args.OPTPNM
  if len(args.FTN15)  > 0: FTN15  = args.FTN15
  if len(args.FTN16)  > 0: FTN16  = args.FTN16
  if len(args.SOF1)   > 0: SOF1   = args.SOF1


  # Argument list for arguments to be tagged onto the
  # end of the command.
  alist = []

  # Argument list for args to be added on to the end
  # of the qsub command.
  qargs = []
  
  jobscr = []
  if os.name == 'nt':
    jobscr.append('@ECHO OFF\n')
    jobscr.append('SETLOCAL\n')
    jobscr.append('set DIRCTY={0}\n'.format(DIRCTY))
    jobscr.append('set RFDIR={0}\n'.format(RFDIR))

    jobscr.append('set NPTPNM={0}\n'.format(NPTPNM))
    jobscr.append('set PLTNM={0}\n'.format(PLTNM))
    jobscr.append('set DICTNM={0}\n'.format(DICTNM))
    jobscr.append('set PUNCHNM={0}\n'.format(PUNCHNM))
    jobscr.append('set OPTPNM={0}\n'.format(OPTPNM))
    jobscr.append('set LOGNM={0}\n'.format(LOGNM))

    jobscr.append('set IN12={0}\n'.format(IN12))
    jobscr.append('set OUT11={0}\n'.format(OUT11))

    jobscr.append('set FTN11={0}\n'.format(FTN11 ))
    jobscr.append('set FTN12={0}\n'.format(FTN12 ))
    jobscr.append('set FTN13={0}\n'.format(FTN13 ))
    jobscr.append('set FTN14={0}\n'.format(FTN14 ))
    jobscr.append('set FTN15={0}\n'.format(FTN15 ))
    jobscr.append('set FTN16={0}\n'.format(FTN16 ))
    jobscr.append('set FTN17={0}\n'.format(FTN17 ))
    jobscr.append('set FTN18={0}\n'.format(FTN18 ))
    jobscr.append('set FTN19={0}\n'.format(FTN19 ))
    jobscr.append('set FTN20={0}\n'.format(FTN20 ))
    jobscr.append('set FTN21={0}\n'.format(FTN21 ))
    jobscr.append('set FTN22={0}\n'.format(FTN22 ))
    jobscr.append('set FTN23={0}\n'.format(FTN23 ))

    jobscr.append('set SOF1={0}\n'.format(SOF1))
    jobscr.append('set SOF2={0}\n'.format(SOF2))

    jobscr.append('set DBMEM={0}\n'.format(12000000))
    jobscr.append('set OCMEM={0}\n'.format(2000000))

    jobscr.append('CHDIR /D "{0}"\n'.format(workdir))
    jobscr.append('MKDIR "{0}"\n'.format(DIRCTY))
    jobscr += '"{0}" "{1}" <"{2}" >"{3}"\n'.format(nastran_x,
                                         ' '.join(alist),
                                             os.path.abspath(args.input),
                                             F06)
    jobscr.append('RMDIR /Q /S "{0}"\n'.format(DIRCTY))
    jobscr.append('ENDLOCAL\n')
    jobscr.append('CALL :DELETESELF & EXIT /B\n')
    jobscr.append(':DELETESELF\n')
    jobscr.append('START /B "" CMD /C DEL /F /Q "%~DPNX0"&EXIT /B\n')
    batchname = os.path.join(workdir,'{0}.bat'.format(rstr))
    batchfile = open(batchname,'w')
    batchfile.writelines(jobscr)
    batchfile.close()
  else:
  #jobscr += '/usr/local/bin/qsub {0} - <<EOF'.format(' '.join(qargs))
    jobscr.append('#!/bin/sh\n')
    jobscr.append('#PBS -q nas\n')
    jobscr.append('#PBS -N {0}\n'.format(bname))
    jobscr.append('#PBS -k n\n')
    jobscr.append('export DIRCTY={0}\n'.format(DIRCTY))
    jobscr.append('export RFDIR={0}\n'.format(RFDIR))

    jobscr.append('export NPTPNM={0}\n'.format(NPTPNM))
    jobscr.append('export PLTNM={0}\n'.format(PLTNM))
    jobscr.append('export DICTNM={0}\n'.format(DICTNM))
    jobscr.append('export PUNCHNM={0}\n'.format(PUNCHNM))
    jobscr.append('export OPTPNM={0}\n'.format(OPTPNM))
    jobscr.append('export LOGNM={0}\n'.format(LOGNM))

    jobscr.append('export IN12={0}\n'.format(IN12))
    jobscr.append('export OUT11={0}\n'.format(OUT11))

    jobscr.append('export FTN11={0}\n'.format(FTN11 ))
    jobscr.append('export FTN12={0}\n'.format(FTN12 ))
    jobscr.append('export FTN13={0}\n'.format(FTN13 ))
    jobscr.append('export FTN14={0}\n'.format(FTN14 ))
    jobscr.append('export FTN15={0}\n'.format(FTN15 ))
    jobscr.append('export FTN16={0}\n'.format(FTN16 ))
    jobscr.append('export FTN17={0}\n'.format(FTN17 ))
    jobscr.append('export FTN18={0}\n'.format(FTN18 ))
    jobscr.append('export FTN19={0}\n'.format(FTN19 ))
    jobscr.append('export FTN20={0}\n'.format(FTN20 ))
    jobscr.append('export FTN21={0}\n'.format(FTN21 ))
    jobscr.append('export FTN22={0}\n'.format(FTN22 ))
    jobscr.append('export FTN23={0}\n'.format(FTN23 ))

    jobscr.append('export SOF1={0}\n'.format(SOF1))
    jobscr.append('export SOF2={0}\n'.format(SOF2))

    jobscr.append('export DBMEM={0}\n'.format(12000000))
    jobscr.append('export OCMEM={0}\n'.format(2000000))

    jobscr.append('cd "{0}"\n'.format(workdir))
    jobscr.append('mkdir -p "{0}"\n'.format(DIRCTY))
    jobscr += '"{0}" "{1}" <"{2}" >"{3}"\n'.format(nastran_x,
                                         ' '.join(alist),
                                             os.path.abspath(args.input),
                                             F06)
    jobscr.append('rm -rf "{0}"\n'.format(DIRCTY))

    batchname = os.path.join(workdir,'{0}.sh'.format(rstr))
    jobscr.append('rm -rf "{0}"\n'.format(batchname))

    batchfile = open(batchname,'w')
    batchfile.writelines(jobscr)
    batchfile.close()
    os.chmod(batchname, 0755)

  if args.no_run:
    print('{0} created.'.format(batchname))
  else:
    os.system('"{0}"'.format(batchname))

def set_parser(parser):
  parser.set_defaults(func=main)

  parser.add_argument('--version',
                      action='version',
                      version='nesub-{0}'.format(__version__))

  parser.add_argument('-n', '--no-run',
                      action='store_true',
                      default=False,
                      help='print generated job script, but do not run')

  parser.add_argument('-O', '--OPTPNM',
                      metavar='OUTPNM',
                      type=str,
                      default='',
                      help='override OPTPNM (OutPut TaPe NuMber)')

  parser.add_argument('-f15', '--FTN15',
                      metavar='FTN15',
                      type=str,
                      default='',
                      help='override FTN15')

  parser.add_argument('-f16', '--FTN16',
                      metavar='FTN16',
                      type=str,
                      default='',
                      help='override FTN16')

  parser.add_argument('-sof1', '--SOF1',
                      metavar='SOF1',
                      type=str,
                      default='',
                      help='override SOF1')

  parser.add_argument('-o', '--output-dir',
                      metavar='OUTFILESPEC',
                      type=str,
                      default='',
                      help='absolute path of directory to write output')

  parser.add_argument('input',
                      metavar='INPUT',
                      type=str,
                      #nargs='+',
                      help='input file(s)')

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  set_parser(parser)
  args = parser.parse_args()
  args.func(args)
