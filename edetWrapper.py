'''
EDET wrapper
Executes and plots results for an EDET run

NOTE! Type: "%matplotlib auto" in iPython console to 
switch to interactive plots, or "%matplotlib inline" 
to switch to inline, in the console.

Lance Bays
12/2/2017
'''
import os
import matplotlib.pyplot as plt 
import numpy as np 
import itertools
# Define intertools cycle of plot symbols:
marker = itertools.cycle(('o', 'v', 's', '<', '^', '>', '+', 'x', 'h', '*'))

# Start Changing Inputs Here ************************

edetInput = {'acProject': '737-700',
'sRefInFt2': 1341.15, 
'arWing': 9.45,
'tcWingInDecimal': 0.129,
'sweepWingQtrChrdInDeg': 25.0,
'taperWingInDecimal': 0.278,
'swetWingInFt2': 2682.3,
'camberWingInPct': 1.5,
'aiteklConv2Scrit': 2,
'laminarWingUpperInPct': 0.0,
'laminarWingLowerInPct': 0.0,
'miscDeltaCdWing': 0.0000,

# FUSELAGE:
'swetFuseInFt2': 3682.0, # Fuselage inputs *****
'lenFuseInFt': 105.6,
'lenOverDiamFuse': 8.56,
'baseAreaInFt2': 1.0,
'miscDeltaCdFuse': 0.00,

# CRUDFACTOR:
'crudFactor': 0.06, # CR recommends 0.28

# REFERENCE CONDITIONS FOR POLARS 
'refAltInFt': 33000.0,
'refMach': 0.775,

# VERTICAL TAIL
'sWetVtailInFt2': 522.82,
'macVtailInFt': 11.74,
'tcVtailInDecimal': 0.089,
'miscDeltaCdVtail': 0.0000,

# HORIZONTAL TAIL
'sWetHtailInFt2': 697.48,
'macHtailInFt': 9.2,
'tcHtailInDecimal': 0.141,
'miscDeltaCdHtail': 0.0000,

# FLAP TRACK FAIRINGS / MISCELLANEOUS
'sWetMisc1InFt2': 72.12,
'macMisc1InFt': 9.32,
'tcMisc1InDecimal': 0.02,
'miscDeltaCdMisc1': 0.0000

# BE SURE TO WRITE ANY ADDITIONAL COMPONENTS TO INPUT FILE HERE
}
    
# Establish working directory with exe...
# Copy & paste absolute path on Local machine here within double quotes
userExePath = "C:\AeroProgs\EDET\EDET"

# Select number of last (highest) Machs to ship in polar plots
# (usually the last one or two look funky and skew the x-axes) 
numLastMachSkip = 1

# Stop Changing Inputs Here ********************************************

# Split drive Letter from path
drive, exePath = userExePath.split("\\",1)

# Handle case where user doesn't include drive in path—
# we will assume it's on the C drive 
if not drive: drive="C:"

# Open EDET input file for write
fin = open(drive + "\\" + exePath + "\\EDET.in", 'w')
# Write line 1 inputs 
fin.write('* Case: \n')
fin.write('* EDET Input file for: ' + edetInput['acProject'] + '\n')
# Write Line 2 inputs
fin.write('* SREF AR T/C SWEEP TAPER\n') 
fin.write(str(edetInput['sRefInFt2']) + ' ' +
    str(edetInput['arWing']) + ' ' +
    str(edetInput['tcWingInDecimal']) + ' ' +
    str(edetInput['sweepWingQtrChrdInDeg']) + '  ' +
    str(edetInput['taperWingInDecimal']) + '\n')
# Write Line 3 inputs
fin.write('* S_WET CAMBER% AITEK UP_LAM% LW_LAM% DELCD0\n') 
fin.write(str(edetInput['swetWingInFt2']) + ' '+
    str(edetInput['camberWingInPct']) + ' ' +
    str(edetInput['aiteklConv2Scrit']) + ' ' +
    str(edetInput['laminarWingUpperInPct']) + ' ' +
    str(edetInput['laminarWingLowerInPct']) + ' ' +
    str(edetInput['miscDeltaCdWing']) + '\n')
# Write Line 4 inputs 
fin.write('*\n')
fin.write('* FUSELAGE GEOMETRY\n')
fin.write('* S_WET S_LEN BODY_L/D SBASE DELCD0\n') 
fin.write(str(edetInput['swetFuseInFt2']) + '  ' +
    str(edetInput['lenFuseInFt']) + '  ' +
    str(edetInput['lenOverDiamFuse']) + ' ' +
    str(edetInput['baseAreaInFt2']) + '  ' +
    str(edetInput['miscDeltaCdFuse']) + '\n') 
fin.write('*\n') 
fin.write('* CRUDFACTOR\n')
fin.write(str(edetInput['crudFactor']) + '\n') 
fin.write('*\n')
fin.write('*REF_ALT REF_MACH\n') 
fin.write(str(edetInput['refAltInFt']) + ' ' +
          str(edetInput['refMach']) + '\n') 
fin.write('*\n')
# VERTICAL TAIL
fin.write('*Additional component: V-Tail\n') 
fin.write('Vtail\n')
fin.write('*SWET LENGTH TC_OR_FR DELCD0\n')
fin.write(str(edetInput['sWetVtailInFt2']) + ' ' +
    str(edetInput['macVtailInFt']) + ' ' +
    str(edetInput['tcVtailInDecimal']) + ' ' +
    str(edetInput['miscDeltaCdVtail']) + '\n')
# HORIZONTAL TAIL
fin.write('*Additional component: H-Tail\n') 
fin.write('Htail\n')
fin.write('*SWET LENGTH TC_OR_FR DELCD0\n')
fin.write(str(edetInput['sWetHtailInFt2']) + ' ' +
    str(edetInput['macHtailInFt']) + ' ' +
    str(edetInput['tcHtailInDecimal']) + ' ' +
    str(edetInput['miscDeltaCdHtail']) + '\n')
# FLAP TRACK FAIRINGS / MISCELLANEOUS
fin.write('*Additional component: Misc1\n') 
fin.write('Misc1\n')
fin.write('*SWET LENGTH TC_OR_FR DELCD0\n')
fin.write(str(edetInput['sWetMisc1InFt2']) + ' ' +
    str(edetInput['macMisc1InFt']) + ' ' +
    str(edetInput['tcMisc1InDecimal']) + ' ' +
    str(edetInput['miscDeltaCdMisc1']) + '\n')

# BE SURE TO WRITE ANY ADDITIONAL COMPONENTS TO INPUT FILE HERE

# DO NOT FORGET THIS BLANK LINE TO IDENTIFY EOF! 
fin.write(' ')
fin.close()

###############################################################
# Run program

#Command-Line instructions to change drivej directory, and run 
runString = drive + " && cd \\" + exePath + " && edet.exe" 
os.system(runString)

###############################################################
# Read output file
fout = open(drive + "\\" + exePath + "\\EDET.out", 'r') 
lines=fout. readlines()
fout.close()

###############################################################
# Parse results
fig = plt.figure()
# Set reasonable size for when plots appear inline 
fig.set_figheight(15)
fig.set_figwidth(20)
#Add verticle space between rows of subplots 
fig.subplots_adjust(hspace=0.4)

# ***** Plot polars
polarList = lines[lines.index(" *MACH         ALPHA        CL        CD\n") +1:
	lines.index(" *    MACH       CDF       CDC BUFFET_CL\n") - 1]
# Convert to numpy array 
polarData=[]
for index, line in enumerate(polarList):
	polarData.append(np.array(list(map(float, polarList[index].split()))))
polarData=np.array(polarData)
# Determine array of unique Mach numbers for plot
plotMachs = np.unique(polarData[0:,0:1])
# Add subplot for polars
p1 = fig.add_subplot(3,2,1)
for index in plotMachs[:-numLastMachSkip]:
	p1.plot(polarData[polarData[:,0]==index]
	[:,np.array([False,False,False,True])],polarData[polarData[:,0]==index]
	[:,np.array([False,False,True,False])],marker=next(marker), label=index)
p1.legend(title='Mach', ncol=2, prop={'size': 10}) 
p1.grid()
p1.set(ylabel='$C_L$',
	xlabel='$C_D$',
	title=edetInput['acProject'] + ' Drag Polars (Ref M='+ 
   str(edetInput['refMach']) + ', Alt=' +
	str(int(edetInput['refAltInFt'])) + ' ft)')
p1.xaxis.label.set_size(18)
p1.yaxis.label.set_size(18)


# ******* Plot AOA
p2 = fig.add_subplot(3,2,2)
for index in plotMachs[:-numLastMachSkip]:
	p2.plot(polarData[polarData[:,0]==index]
	[:,np.array([False,True,False,False])],polarData[polarData[:,0]==index]
	[:,np.array([False,False,True,False])],marker=next(marker), label=index)
p2.legend(title='Mach', ncol=2, prop={'size': 10}) 
p2.grid()
p2.set(ylabel='$C_L$',
	xlabel=r'$\alpha$' + ' (deg)',
	title=edetInput['acProject'] + ' AOA')
p2.xaxis.label.set_size(18)
p2.yaxis.label.set_size(18)


# ******** Plot zero lift drag 
dragBuffetList=lines[lines.index(" *    MACH       CDF       CDC BUFFET_CL\n")+
	1:lines.index(" * INDUCED DRAG\n") - 1]
# Convert to numpy array
dragBuffetData=[]
for index, line in enumerate(dragBuffetList):
	dragBuffetData.append(np.array(list(map(float,
	dragBuffetList[index].split()))))
dragBuffetData=np.array(dragBuffetData)
p3 = fig.add_subplot(3,2,3)
p3.plot(dragBuffetData[:,0], dragBuffetData[:,1], '-o', label='$C_{Df}$') 
p3.plot(dragBuffetData[:,0], dragBuffetData[:,2], '-v', label='$C_{Dc}$') 
p3.plot(dragBuffetData[:,0], dragBuffetData[:,1] + dragBuffetData[:,2], '-s',
	label='$C_{DO}$')
p3.legend(prop={'size': 16}) 
p3.grid()
p3.set(ylabel='$C_D$',
	xlabel='$Mach$', 
	title=edetInput['acProject'] + ' Zero Lift Drag') 
p3.xaxis.label.set_size(18)
p3.yaxis.label.set_size(18)


# ************ Plot buffet onset
p4 = fig.add_subplot(3,2,4)
p4.plot(dragBuffetData[:,0], dragBuffetData[:,3], '-o', label='$C_{L_{BO}}$')
p4.legend(prop={'size': 16})
p4.grid()
p4.set(ylabel='$C_{L_{BO}}$', 
	xlabel='$Mach$', 
	title=edetInput['acProject'] + ' Buffet Onset') 
p4.xaxis.label.set_size(18)
p4.yaxis.label.set_size(18)


# *********** Plot L/D
# Compute L/D. Note: this overwrites CD with L/D in fourth column
polarData[:,3] = polarData[:,2] / polarData[:,3]
p5 = fig.add_subplot(3,2,5)
for index in plotMachs[:-numLastMachSkip]:
	p5.plot(polarData[polarData[:,0]==index]
	[:,np.array([False,False,True,False])],polarData[polarData[:,0]==index]
	[:,np.array([False,False,False,True])],marker=next(marker), label=index)
p5.legend(title='Mach', ncol=2, prop={'size': 10}) 
p5.grid()
p5.set(ylabel='$L/D$',
	xlabel='$C_L$',
	xlim=(0,1),
	title=edetInput['acProject'] + ' L/D (Ref M='+ 
   str(edetInput['refMach']) + ', Alt=' +
	str(int(edetInput['refAltInFt'])) + ' ft)')
p5.xaxis.label.set_size(18)
p5.yaxis.label.set_size(18)


# ****** Plot Re Corrn
reList = lines[lines.index(" * ALTITUDE     MACH  DELTA_CD\n") + 1:
	lines.index(" *MACH         ALPHA        CL        CD\n") - 3]
# Convert to numpy array 
reData=[]
for index, line in enumerate(reList):
	reData.append(np.array(list(map(float, reList[index].split()))))
reData=np.array(reData)
# Determine array of unique Mach numbers for plot
reAlts = np.unique(reData[0:,0:1])
# Add subplot for Re corrn 
p6 = fig.add_subplot(3,2,6) 
for index in reAlts:
	p6.plot(reData[reData[:,0]==index]
	[:,np.array([False,True,False])],reData[reData[:,0]==index]
	[:,np.array([False,False,True])], marker=next(marker),label=int(index/1000))
p6.legend(title='Altitude (1000 ft)', ncol=2, prop={'size': 10})
p6.grid()
p6.set(ylabel=r'$\Delta$$C_{D_{RE}}$', 
	xlabel='$Mach$',
	xlim=(0,1),
	title = edetInput['acProject'] + ' Re# Correction (Ref M=' +
       str(edetInput['refMach']) + ', Alt=' +
		str(int(edetInput['refAltInFt'])) + ' ft)')
p6.xaxis.label.set_size(18)
p6.yaxis.label.set_size(18)