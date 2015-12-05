from psychopyHelp import *
hz=120.0

myDlg = gui.Dlg(title="Setting experiment"); myDlg.addField('Observer:','dl')
myDlg.show(); expInfo = myDlg.data
dataFile=openDataFile(expInfo[0])

win = visual.Window(monitor='testMonitor',allowGUI=False,units='deg',fullscr=1)
#win = visual.Window(monitor='macprotracking',allowGUI=False,units='deg',fullscr=1)
fixation=visual.PatchStim(win, tex=None, mask='gauss', color=-1,size=.5)
sti=visual.PatchStim(win, tex='None', mask='gauss')
landmark=visual.PatchStim(win, tex='None', mask='gauss',color=-1)
fixation.setAutoDraw(True)

vars={'angleIni': arange(0.0,360.0,60.0),'radius':[3.0],'freq':arange(0.75,3.75,0.25),'direction':[-1,1],'size':[2.0],
           'angleLandmark':arange(0.0,360.0,45.0),'radiusLandmark':[4.75], 'sizeLandmark':[2.0],
            'durationIni':[1.0*hz],'durationRampingIni':[.75*hz],'duration':[2.0*hz],
            'durationRampingFinal':[.75*hz],'durationFinal':[1.0*hz],
            'same':[True,False]}
stimList = createList(vars)
trials = data.TrialHandler(stimList,1)
print trials.nTotal
nDone=0

for thisTrial in trials:
    sti.setSize(thisTrial['size'])
    landmark.setSize(thisTrial['sizeLandmark'])
    r=thisTrial['radius']
    durationTrial= thisTrial['durationIni']+thisTrial['durationRampingIni']+thisTrial['duration'] + thisTrial['durationRampingFinal']+thisTrial['durationFinal'] 
    win.setRecordFrameIntervals(True)
    
    for frame in range(int(durationTrial)):
        sti.setColor(1)
        angle=thisTrial['angleIni']+thisTrial['direction']*frame/hz*thisTrial['freq']*360.0
        anglerad=angle/180.0*pi
        x=r*cos(anglerad)
        y=r*sin(anglerad)
        t1=thisTrial['durationIni']
        t2=thisTrial['durationIni']+thisTrial['durationRampingIni']
        t3=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['duration']
        t4=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['duration']+thisTrial['durationRampingFinal']
        
        if frame <= t1:
            sti.setPos([x,y]); sti.draw()
        if frame > t1 and frame <=t2 :
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            lum=(frame-t1)/thisTrial['durationRampingIni']
            sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
        if frame > t2 and frame <= t3:
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            sti.setPos([-x,-y]); sti.draw()
        if frame > t3 and frame <= t4:
            lum=1-(frame-t3)/thisTrial['durationRampingFinal']
            if thisTrial['same']:
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
                sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
            else:
               sti.setColor(lum); sti.setPos([x,y]); sti.draw()
               sti.setColor(1); sti.setPos([-x,-y]); sti.draw()
        if frame > t4:
            if thisTrial['same']:
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
            else:
               sti.setColor(1); sti.setPos([-x,-y]); sti.draw()
        rLandmark=thisTrial['radiusLandmark']
        angleLandmark=thisTrial['angleLandmark']
        angleLandmarkrad=angleLandmark/180.0*pi
        xLandmark=rLandmark*cos(angleLandmarkrad)
        yLandmark=rLandmark*sin(angleLandmarkrad)
        landmark.setPos([xLandmark,yLandmark]); landmark.draw()
        win.flip()
        esc()
    
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
