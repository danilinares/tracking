from __future__ import division  # so that 1/3=0.333 instead of 1/3=0
from psychopy import *
from numpy import *
from random import *
import time
from itertools import *
import os

def repite(frame,duration,numberStates=2,start=1): ### Creating repetitive stimuli
    startP=numberStates*duration-start%(numberStates*duration)
    counter=(frame+startP)%(numberStates*duration)
    for i in range(numberStates):
        if counter>=i*duration and counter<(i+1)*duration:
            flag=i+1
    return flag
def getResponse(keys):
    thisResp=None
    while thisResp==None:
        allKeys=event.waitKeys()
        for thisKey in allKeys:
            for k in keys:
                if thisKey==k:
                    response=thisKey
            if thisKey in ['q', 'escape']:
                core.quit()
        return(response)            

def openDataFile(subject):
    if not os.path.exists('data'):
        os.makedirs('data')
    timeAndDateStr = time.strftime("%d%b%Y_%H-%M", time.localtime()) 
    dataFile=open('data/' + subject + timeAndDateStr  + '.txt', 'w')
    return dataFile
def esc():
    for keys in event.getKeys():
        if keys in ['escape','q']:
            core.quit() 
def createList(dicts):
    return list(dict(izip(dicts, x)) for x in product(*dicts.itervalues()))