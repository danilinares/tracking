from psychopyHelp import *

myDlg = gui.Dlg(title="Setting experiment")
myDlg.addField('Observer:','dl')
myDlg.show()
expInfo = myDlg.data
dataFile=openDataFile(expInfo[0])

win = visual.Window(monitor='testMonitor',allowGUI=False,units='deg')
fixation=visual.PatchStim(win, tex=None, mask='circle', color=-1,size=.5)
sti=visual.PatchStim(win, tex='sin', mask='circle', color=.5,size=4)
fixation.setAutoDraw(False)

vars={'orientation': [1], 'location' :[0,1,-1]}
stimList = createList(vars)
trials = data.TrialHandler(stimList,20)

nDone=0
for thisTrial in trials:
    win.setRecordFrameIntervals(True)
    for frame in range(6):
        sti.setPos([thisTrial['location']*4,0])
        sti.setOri(thisTrial['orientation'])
        sti.draw()
        win.flip()
    for frame in range(20):
        win.flip()
    for frame in range(6):
        sti.setPos([thisTrial['location']*4,0])
        sti.setOri(thisTrial['orientation']-1)
        sti.draw()
        win.flip()
    win.flip()
    win.setRecordFrameIntervals(False)
    response=getResponse(['left','right'])
    
    nDone+=1
    if nDone==1:
        print >>dataFile, 'trial', 
        for name in thisTrial.keys():
            print>>dataFile,name,
        print >>dataFile, 'response'

    print >>dataFile, nDone,
    for value in thisTrial.values():
        print>>dataFile, value,
    print>>dataFile, response
    
dataFile.close()
core.quit()
