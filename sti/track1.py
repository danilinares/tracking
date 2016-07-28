from psychopyHelp import *
hz=120.0

myDlg = gui.Dlg(title="Setting experiment"); myDlg.addField('Observer:','dl'); myDlg.addField('task (track/press):','press')
myDlg.show(); expInfo = myDlg.data
dataFile=openDataFile(expInfo[0])
task=expInfo[1]

win = visual.Window(monitor='testMonitor',allowGUI=False,units='deg')
#win = visual.Window(monitor='macprotracking',allowGUI=False,units='deg',fullscr=1)
fixation=visual.PatchStim(win, tex=None, mask='gauss', color=-1,size=1)
sti=visual.PatchStim(win, tex='None', mask='gauss')
landmark=visual.PatchStim(win, tex='None', mask='gauss',color=-1)
myMouse = event.Mouse(); myMouse.setVisible(0)
fixation.setAutoDraw(True)

vars={'radius':[4.0], 'freq':arange(0.75,3.75,0.25),
            'direction':[-1,1],'size':[3.0],
            'angleLandmark':arange(0.0,360.0,45.0),'radiusLandmark':[6.5], 'sizeLandmark':[3.0],
            'durationIni':[1.0*hz],'durationRampingIni':[.75*hz],'durationRampingLand':[1.0*hz],'duration':[1.5*hz],
            'durationRampingFinal':[.75*hz],'durationFinal':[1.0*hz],
            'same':[True,False]}
stimList = createList(vars)
trials = data.TrialHandler(stimList,1)
print trials.nTotal
nDone=0

for thisTrial in trials:
    response=None
    sti.setSize(thisTrial['size'])
    landmark.setSize(thisTrial['sizeLandmark'])
    r=thisTrial['radius']
    durationTrial= thisTrial['durationIni']+thisTrial['durationRampingIni']+thisTrial['durationRampingLand']+thisTrial['duration'] + thisTrial['durationRampingFinal']+thisTrial['durationFinal'] 
    win.setRecordFrameIntervals(True)
    angleIni=randint(0,359)
    
    rLandmark=thisTrial['radiusLandmark']
    angleLandmark=thisTrial['angleLandmark']
    angleLandmarkrad=angleLandmark/180.0*pi
    xLandmark=rLandmark*cos(angleLandmarkrad)
    yLandmark=rLandmark*sin(angleLandmarkrad)
    landmark.setPos([xLandmark,yLandmark])
    
    myMouse.clickReset()
    for frame in range(int(durationTrial)):
        sti.setColor(1)
        angle=angleIni+thisTrial['direction']*frame/hz*thisTrial['freq']*360.0
        anglerad=angle/180.0*pi
        x=r*cos(anglerad)
        y=r*sin(anglerad)
        t1=thisTrial['durationIni']
        t2=thisTrial['durationIni']+thisTrial['durationRampingIni']
        t3=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand']
        t4=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand'] +thisTrial['duration']
        t5=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand'] +thisTrial['duration']+thisTrial['durationRampingFinal']
        
        if frame <= t1:
            sti.setPos([x,y]); sti.draw()
        if frame > t1 and frame <=t2 :
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            lum=(frame-t1)/thisTrial['durationRampingIni']
            sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
        if frame > t2 and frame <= t3:
            lumLand=(frame-t2)/thisTrial['durationRampingLand']
            landmark.setColor(-lumLand); landmark.draw()
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            sti.setPos([-x,-y]); sti.draw()
        if frame > t3 and frame <= t4:
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            sti.setPos([-x,-y]); sti.draw()
            landmark.draw()
        if frame > t4 and frame <= t5:
            lum=1-(frame-t4)/thisTrial['durationRampingFinal']
            landmark.draw()
            if thisTrial['same']:
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
                sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
            else:
               sti.setColor(lum); sti.setPos([x,y]); sti.draw()
               sti.setColor(1); sti.setPos([-x,-y]); sti.draw()
        if frame > t5:
            landmark.draw()
            if thisTrial['same']:
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
            else:
               sti.setColor(1); sti.setPos([-x,-y]); sti.draw()

        win.flip()
        esc()

        if task=='press':
            buttons, times = myMouse.getPressed(getTime=True)
            if buttons[0]==1 and response==None:
                response=angle
                responseTime=times[0]

    win.flip()
    win.setRecordFrameIntervals(False)
    
    if task=='track':
        response=getResponse(['left','right'])
    
    nDone+=1
    if nDone==1:
        print >>dataFile, 'trial', 'angleIni',
        for name in thisTrial.keys():
            print>>dataFile,name,
        if task=='tracking':
            print >>dataFile, 'response'
        if task=='press':
            print >>dataFile, 'response',
            print >>dataFile, 'responseTime'
        
    print >>dataFile, nDone, angleIni,
    for value in thisTrial.values():
        print>>dataFile, value,
    if task=='tracking':
        print >>dataFile, response
    if task=='press':
        if response==None:
            responseTime=None
        print >>dataFile, response,
        print >>dataFile, responseTime
    
dataFile.close()
core.quit()
