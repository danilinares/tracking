from psychopyHelp import *
from math import pi
hz=120.0
# 1024*768
myDlg = gui.Dlg(title="Setting experiment"); myDlg.addField('Observer:','lk')
myDlg.show(); expInfo = myDlg.data
dataFile=openDataFile(expInfo[0])
#task="press"
#task="tracking"
task="phase"

win = visual.Window(monitor='testMonitor',allowGUI=False,units='deg',fullscr=1)
#win = visual.Window(monitor='macprotracking',allowGUI=False,units='deg',fullscr=1)
fixation=visual.PatchStim(win, tex=None, mask='gauss', color=-1,size=1)
sti=visual.PatchStim(win, tex='None', mask='gauss')
landmark=visual.PatchStim(win, tex='None', mask='gauss',color=-1)
myMouse = event.Mouse(); myMouse.setVisible(0)
fixation.setAutoDraw(True)

vars={'radius':[4.0], 'freq': arange(2.0,3.75,0.25),
            'direction':[-1,1],'size':[3.0],'radiusLandmark':[6.5], 'sizeLandmark':[3.0],
            'durationIni':[1.0*hz],'durationRampingIni':[0.75*hz],'durationRampingLand':[1.0*hz],'duration':[1.5*hz],
            'durationRampingFinal':[.75*hz],'durationFinal':[0.0*hz], # 1.0 for tracking and press
            'same':[True,False]}
stimList = createList(vars)
trials = data.TrialHandler(stimList,3)
print trials.nTotal
nDone=0

for thisTrial in trials:
    response=None
    sti.setSize(thisTrial['size'])
    landmark.setSize(thisTrial['sizeLandmark'])
    r=thisTrial['radius']
    r2=r+3.0
    durationTrial= thisTrial['durationIni']+thisTrial['durationRampingIni']+thisTrial['durationRampingLand']+thisTrial['duration'] + thisTrial['durationRampingFinal']+thisTrial['durationFinal'] 
    win.setRecordFrameIntervals(True)
    angleIni=random()*360.0
    
    rLandmark=thisTrial['radiusLandmark']
    angleLandmark=random()*360.0
    angleLandmarkrad=angleLandmark/180.0*pi
    xLandmark=rLandmark*cos(angleLandmarkrad)
    yLandmark=rLandmark*sin(angleLandmarkrad)
    landmark.setPos([xLandmark,yLandmark])
    
    myMouse.clickReset()
    for frame in range(int(durationTrial)):
        sti.setColor(1)
        angle=angleIni+thisTrial['direction']*frame/hz*thisTrial['freq']*360.0
        anglerad=angle/180.0*pi
        if thisTrial['same']:
            pha = 0.0
        else:
            pha = pi / 2.0
        x=r*cos(anglerad)
        y=r*sin(anglerad)
        x2=r2*cos(anglerad + pha)
        y2=r2*sin(anglerad + pha)
        t1=thisTrial['durationIni']
        t2=thisTrial['durationIni']+thisTrial['durationRampingIni']
        t3=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand']
        t4=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand'] +thisTrial['duration']
        t5=thisTrial['durationIni']+thisTrial['durationRampingIni']+ thisTrial['durationRampingLand'] +thisTrial['duration']+thisTrial['durationRampingFinal']
        
        if frame <= t1:
            if task=="tracking":
                sti.setPos([x,y]); sti.draw()
        if frame > t1 and frame <=t2 :
            if task=="tracking":
                lum=(frame-t1)/thisTrial['durationRampingIni']
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
                sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
            if task=="press":
                lum=1
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
                sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
            if task=="phase":
                lum=(frame-t1)/thisTrial['durationRampingIni']
                sti.setColor(lum); 
                sti.setPos([x,y]); sti.draw()
                sti.setPos([-x,-y]); sti.draw()
                sti.setPos([x2,y2]); sti.draw()
                sti.setPos([-x2,-y2]); sti.draw()
        if frame > t2 and frame <= t3:
            if task=="tracking" or task=="press":
                lumLand=(frame-t2)/thisTrial['durationRampingLand']
                landmark.setColor(-lumLand); landmark.draw()
            sti.setColor(1); 
            sti.setPos([x,y]); sti.draw()
            sti.setPos([-x,-y]); sti.draw()
            if task=="phase":
                sti.setPos([x2,y2]); sti.draw()
                sti.setPos([-x2,-y2]); sti.draw()
        if frame > t3 and frame <= t4:
            sti.setColor(1); sti.setPos([x,y]); sti.draw()
            sti.setPos([-x,-y]); sti.draw()
            if task=="phase":
              sti.setPos([x2,y2]); sti.draw()
              sti.setPos([-x2,-y2]); sti.draw()
            if task=="tracking" or task=="press":
                landmark.draw()
        if frame > t4 and frame <= t5:
            if task=="tracking":
                lum=1-(frame-t4)/thisTrial['durationRampingFinal']
            else:
                lum=1
            if task=="tracking" or task=="press":
                landmark.draw()
                if thisTrial['same']:
                    sti.setColor(1); sti.setPos([x,y]); sti.draw()
                    sti.setColor(lum); sti.setPos([-x,-y]); sti.draw()
                else:
                   sti.setColor(lum); sti.setPos([x,y]); sti.draw()
                   sti.setColor(1); sti.setPos([-x,-y]); sti.draw()
            if task=="phase":
              lum=1-(frame-t4)/thisTrial['durationRampingFinal']
              sti.setColor(lum)
              sti.setPos([x,y]); sti.draw()
              sti.setPos([-x,-y]); sti.draw()
              sti.setPos([x2,y2]); sti.draw()
              sti.setPos([-x2,-y2]); sti.draw()
        if frame > t5:
            if task=="tracking" or task=="press":
                landmark.draw()
            if task=="tracking":
                if thisTrial['same']:
                    sti.setColor(1); sti.setPos([x,y]); sti.draw()
                else:
                   sti.setColor(1); sti.setPos([-x,-y]); sti.draw()
            if task=="press":
                sti.setColor(1); sti.setPos([x,y]); sti.draw()
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
    
    if task=='tracking' or task=='phase':
        response=getResponse(['left','right'])
    
    nDone+=1
    if nDone==1:
        print >>dataFile, 'trial', 'angleIni', 'angleLandmark',
        for name in thisTrial.keys():
            print>>dataFile,name,
        if task=='tracking' or task=='phase':
            print >>dataFile, 'response'
        if task=='press':
            print >>dataFile, 'response',
            print >>dataFile, 'responseTime'
        
    print >>dataFile, nDone, angleIni, angleLandmark,
    for value in thisTrial.values():
        print>>dataFile, value,
    if task=='tracking' or task=='phase':
        print >>dataFile, response
    if task=='press':
        if response==None:
            responseTime=None
        print >>dataFile, response,
        print >>dataFile, responseTime
    
dataFile.close()
core.quit()
