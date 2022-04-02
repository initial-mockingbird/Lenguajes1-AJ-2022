import matplotlib.pyplot as plt
import pandas as pd

plotsPath = "./imgs/"
csvPath   = "./csv/"
def plotBig():

    df = pd.read_csv(csvPath + 'times.csv')
    plt.style.use('ggplot')
    plt.figure()

    ax = plt.gca()

    
    plt.xlabel('size')
    plt.ylabel('Avg time (s) per 100 runs')

    df.plot(kind='line',x='size',y='time F', color='blue', ax=ax)
    df.plot(kind='line',x='size',y='time F_tail', color='red', ax=ax)
    df.plot(kind='line',x='size',y='time F_loop', color='green', ax=ax)

    plt.savefig(plotsPath + 'big.PNG')

def plotShortened():
    df = pd.read_csv(csvPath + 'timesShortened.csv')
    plt.style.use('ggplot')
    plt.figure()

    ax = plt.gca()

    
    plt.xlabel('size')
    plt.ylabel('Avg time (s) per 100 runs')

    df.plot(kind='line',x='size',y='time F', color='blue', ax=ax)
    df.plot(kind='line',x='size',y='time F_tail', color='red', ax=ax)
    df.plot(kind='line',x='size',y='time F_loop', color='green', ax=ax)

    plt.savefig(plotsPath + 'short.PNG')

def plotNoRec():
    df = pd.read_csv(csvPath + 'timesNoRec.csv')
    plt.style.use('ggplot')
    plt.figure()

    ax = plt.gca()


    plt.xlabel('size')
    plt.ylabel('Avg time (s) per 100 runs')


    df.plot(kind='line',x='size',y='time F_tail', color='red', ax=ax)
    df.plot(kind='line',x='size',y='time F_loop', color='green', ax=ax)

    plt.savefig(plotsPath + 'noRec.PNG')

if __name__=="__main__":
    plotBig()
    plotShortened()
    plotNoRec()
