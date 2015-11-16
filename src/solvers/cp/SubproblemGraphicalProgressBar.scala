package solvers.cp

import java.awt._
import javax.swing._

import misc.SimpleMovingAverage
import misc.TimeHelper._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy
import org.jfree.data.xy.XYSeries

import scala.collection.mutable
import scala.swing.Swing

/**
  * Created by dervalguillaume on 15/11/15.
  */
class SubproblemGraphicalProgressBar[T](nbSubproblems: Int, nbThreads: Int) extends JPanel(new BorderLayout()) with Watcher[T] {
  private val progressBar: JProgressBar = new JProgressBar(0, 100)
  private val subproblemStatus: JLabel = new JLabel("")
  private val timeRemaining: JLabel = new JLabel("")
  private val timeElapsed: JLabel = new JLabel("")
  private val timeTotal: JLabel = new JLabel("")
  private val startTime = getClockTime

  /*
    CHARTS
   */
  private val totalClockTimePlotPoints = new XYSeries("Clock")
  private val totalCPUTimePlotPoints = new XYSeries("CPU")

  private val meanClockTimePlotPoints = new XYSeries("Mean (Clock)")
  private val meanCPUTimePlotPoints = new XYSeries("Mean (CPU)")
  private val instantMeanCPUTimePlotPoints = new XYSeries("Simple moving mean (size 10) (CPU)")

  private val collectionTotal = new xy.XYSeriesCollection()
  private val collectionMean = new xy.XYSeriesCollection()
  collectionTotal.addSeries(totalClockTimePlotPoints)
  collectionTotal.addSeries(totalCPUTimePlotPoints)
  collectionMean.addSeries(meanClockTimePlotPoints)
  collectionMean.addSeries(meanCPUTimePlotPoints)
  collectionMean.addSeries(instantMeanCPUTimePlotPoints)

  private val totalTimeChart = ChartFactory.createXYLineChart("", "Clock time", "Total time estimated",
    collectionTotal, PlotOrientation.VERTICAL, true, true, false)
  private val meanTimeChart = ChartFactory.createXYLineChart("", "Clock time", "Time taken per subproblem",
    collectionMean, PlotOrientation.VERTICAL, true, true, false)

  private val totalTimeChartPanel: ChartPanel = new ChartPanel( totalTimeChart )
  totalTimeChartPanel.setPreferredSize( new java.awt.Dimension( 560 , 367 ) )
  private val meanTimeChartPanel: ChartPanel = new ChartPanel( meanTimeChart )
  meanTimeChartPanel.setPreferredSize( new java.awt.Dimension( 560 , 367 ) )

  /*
    INITIALISE GUI
   */
  progressBar.setValue(0)
  progressBar.setStringPainted(true)

  private val panel = new JPanel(new GridLayout(0,2))
  panel.add(new JLabel("Subproblems: ", SwingConstants.LEFT))
  panel.add(subproblemStatus)
  panel.add(new JLabel("Estimated remaining time: ", SwingConstants.LEFT))
  panel.add(timeRemaining)
  panel.add(new JLabel("Elapsed time: ", SwingConstants.LEFT))
  panel.add(timeElapsed)
  panel.add(new JLabel("Total estimated time: ", SwingConstants.LEFT))
  panel.add(timeTotal)
  add(progressBar, BorderLayout.NORTH)
  add(panel, BorderLayout.SOUTH)
  add(totalTimeChartPanel, BorderLayout.WEST)
  add(meanTimeChartPanel, BorderLayout.EAST)

  setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20))

  private def updateProgressBar(): Unit = {
    val percentageSPDone = (subproblemsDone.toDouble/nbSubproblems.toDouble)*100.0
    progressBar.setValue(percentageSPDone.toInt)
  }

  private def updateSubproblemStatus(): Unit = {
    subproblemStatus.setText(""+subproblemsDone+"/"+nbSubproblems)
  }

  private def updateTime(): Unit = {
    val elapsedRealTime: Double = getClockTime-startTime
    val meanRealTime = elapsedRealTime/subproblemsDone.toDouble
    val remainingRealTime = (nbSubproblems - subproblemsDone).toDouble*meanRealTime

    val elapsedCPUTime: Double = subproblemsTotalCPUTime
    val meanCPUTime: Double = subproblemsTotalCPUTime/subproblemsDone.toDouble
    val remainingCPUTime: Double = (nbSubproblems - subproblemsDone).toDouble*meanCPUTime

    timeRemaining.setText(remainingRealTime/math.pow(10, 9)+"s")
    timeElapsed.setText(elapsedRealTime/math.pow(10, 9)+"s")
    timeTotal.setText((elapsedRealTime+remainingRealTime)/math.pow(10, 9)+"s")

    totalClockTimePlotPoints.add(elapsedRealTime/math.pow(10, 9), (elapsedRealTime+remainingRealTime)/math.pow(10, 9))
    totalCPUTimePlotPoints.add(elapsedRealTime/math.pow(10, 9), (elapsedCPUTime+remainingCPUTime)/math.pow(10, 9))

    meanClockTimePlotPoints.add(elapsedRealTime/math.pow(10, 9), meanRealTime/math.pow(10, 9))
    meanCPUTimePlotPoints.add(elapsedRealTime/math.pow(10, 9), meanCPUTime/math.pow(10, 9))
    instantMeanCPUTimePlotPoints.add(elapsedRealTime/math.pow(10, 9), instantMeanTime.get)
  }

  //subproblemsTime contains:
  // - for done subproblems: the time taken to solve them (cpu)
  // - for not started subproblems: 0.0
  // - for started but not yet solved subproblems: the solve start time (clock)
  private val subproblemsTime = Array.tabulate(nbSubproblems)(i => 0.0)
  private var subproblemsDone = 0
  private val subproblemsResolving = new mutable.HashSet[Int]
  private val instantMeanTime = new SimpleMovingAverage(10)
  private var subproblemsTotalCPUTime = 0.0

  override def startedSubproblem(spid: Int): Unit = {
    subproblemsTime(spid) = getClockTime
    subproblemsResolving += spid
  }

  override def newSolution(solution: T): Unit = {}


  override def endedSubproblem(spid: Int, timeTaken: Double): Unit = {
    subproblemsTime(spid) = timeTaken
    subproblemsResolving -= spid
    subproblemsDone += 1
    instantMeanTime.update(timeTaken/Math.pow(10, 9))
    subproblemsTotalCPUTime += timeTaken

    Swing.onEDT {
      updateProgressBar()
      updateSubproblemStatus()
      updateTime()
    }
  }

  override def allDone(): Unit = {}
}

object SubproblemGraphicalProgressBar {
  def apply[T](nbSubproblems: Int, nbThreads: Int, windowName: String = "Progress Bar"): SubproblemGraphicalProgressBar[T] = {
    var gpb:SubproblemGraphicalProgressBar[T] = null

    Swing.onEDTWait {
      val frame = new JFrame(windowName)
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      val newContentPane = new SubproblemGraphicalProgressBar[T](nbSubproblems, nbThreads)
      newContentPane.setOpaque(true)
      frame.setContentPane(newContentPane)
      frame.pack()
      frame.setVisible(true)
      gpb = newContentPane
    }

    gpb
  }
}
