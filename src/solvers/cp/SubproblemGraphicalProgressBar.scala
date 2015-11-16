package solvers.cp

import java.awt._
import javax.swing._

import misc.TimeHelper._
import misc.{FixedBinsHistogramDataset, SimpleMovingAverage}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy
import org.jfree.data.xy.XYSeries

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.swing.Swing

/**
  * A GUI that shows various stats about a multithreaded solve
  * @param nbSubproblems
  * @param nbThreads
  * @tparam T
  */
class SubproblemGraphicalProgressBar[T](nbSubproblems: Int, nbThreads: Int) extends JPanel() with Watcher[T] {
  this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  private val progressBar: JProgressBar = new JProgressBar(0, nbSubproblems)
  private val subproblemStatus: JLabel = new JLabel("")
  private val cpuWallRatio: JLabel = new JLabel("")
  private val wallTimeRemaining: JLabel = new JLabel("")
  private val cpuTimeRemaining: JLabel = new JLabel("")
  private val wallTimeElapsed: JLabel = new JLabel("")
  private val cpuTimeElapsed: JLabel = new JLabel("")
  private val wallTimeTotal: JLabel = new JLabel("")
  private val cpuTimeTotal: JLabel = new JLabel("")
  private val solutionsFoundLabel: JLabel = new JLabel("0")

  private val startTime = getClockTime
  private val timeStorage = new ArrayBuffer[(Double, Double, Double, Double)]
  private var solutionsFound = 0

  /*
    CHARTS
   */
  private val totalWallTimePlotPoints = new XYSeries("Wall")
  private val totalCPUTimePlotPoints = new XYSeries("CPU")

  private val meanWallTimePlotPoints = new XYSeries("Mean (Wall)")
  private val meanCPUTimePlotPoints = new XYSeries("Mean (CPU)")
  private val instantMeanCPUTimePlotPoints = new XYSeries("Simple moving mean (size 10) (CPU)")

  private val subproblemCPUTimeHistogram = new FixedBinsHistogramDataset("CPU", 50)

  private val collectionTotal = new xy.XYSeriesCollection()
  private val collectionMean = new xy.XYSeriesCollection()
  collectionTotal.addSeries(totalWallTimePlotPoints)
  collectionTotal.addSeries(totalCPUTimePlotPoints)
  collectionMean.addSeries(meanWallTimePlotPoints)
  collectionMean.addSeries(meanCPUTimePlotPoints)
  collectionMean.addSeries(instantMeanCPUTimePlotPoints)

  private val totalTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Total time estimated",
    collectionTotal, PlotOrientation.VERTICAL, true, true, false)
  private val meanTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Time taken per subproblem",
    collectionMean, PlotOrientation.VERTICAL, true, true, false)
  private val subproblemCPUTimeChart = ChartFactory.createHistogram("", "CPU Time", "#",
    subproblemCPUTimeHistogram.getRealDataset, PlotOrientation.VERTICAL, true, true, false)

  totalTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  totalTimeChart.setBackgroundImageAlpha(0.0f)
  meanTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  meanTimeChart.setBackgroundImageAlpha(0.0f)
  subproblemCPUTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  subproblemCPUTimeChart.setBackgroundImageAlpha(0.0f)

  private val totalTimeChartPanel: ChartPanel = new ChartPanel(totalTimeChart)
  totalTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val meanTimeChartPanel: ChartPanel = new ChartPanel(meanTimeChart)
  meanTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val subproblemCPUTimeChartPanel: ChartPanel = new ChartPanel(subproblemCPUTimeChart)
  subproblemCPUTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))

  /*
    INITIALISE GUI
   */
  progressBar.setValue(0)
  progressBar.setStringPainted(true)

  private val timePanel = new JPanel(new GridLayout(0, 3))
  timePanel.add(new JLabel("", SwingConstants.LEFT))
  timePanel.add(new JLabel("Wall", SwingConstants.LEFT))
  timePanel.add(new JLabel("CPU", SwingConstants.LEFT))
  timePanel.add(new JLabel("Estimated remaining time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeRemaining)
  timePanel.add(cpuTimeRemaining)
  timePanel.add(new JLabel("Elapsed time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeElapsed)
  timePanel.add(cpuTimeElapsed)
  timePanel.add(new JLabel("Total estimated time: ", SwingConstants.LEFT))
  timePanel.add(wallTimeTotal)
  timePanel.add(cpuTimeTotal)

  private val statPanel = new JPanel(new GridLayout(0, 2))
  statPanel.add(new JLabel("Subproblems: ", SwingConstants.LEFT))
  statPanel.add(subproblemStatus)
  statPanel.add(new JLabel("Solutions found: ", SwingConstants.LEFT))
  statPanel.add(solutionsFoundLabel)
  statPanel.add(new JLabel("CPU/Wall ratio: ", SwingConstants.LEFT))
  statPanel.add(cpuWallRatio)

  private val chartPanel = new JPanel(new GridLayout(2, 2))
  chartPanel.add(totalTimeChartPanel)
  chartPanel.add(meanTimeChartPanel)
  chartPanel.add(subproblemCPUTimeChartPanel)

  add(progressBar)
  add(chartPanel)
  add(statPanel)
  add(timePanel)


  setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20))

  private def updateProgressBar(): Unit = {
    progressBar.setValue(subproblemsDone)
  }

  private def updateSubproblemStatus(): Unit = {
    subproblemStatus.setText("" + subproblemsDone + "/" + nbSubproblems)
  }

  private def updateSolutionsFound(): Unit = {
    solutionsFoundLabel.setText(solutionsFound.toString)
  }

  private def round1000(v: Double): Double = (v * 1000).toInt.toDouble / 1000

  private def updateTime(): Unit = {
    val elapsedWallTime: Double = getClockTime - startTime
    val meanWallTime = elapsedWallTime / subproblemsDone.toDouble
    val remainingWallTime = (nbSubproblems - subproblemsDone).toDouble * meanWallTime

    val elapsedCPUTime: Double = subproblemsTotalCPUTime
    val meanCPUTime: Double = subproblemsTotalCPUTime / subproblemsDone.toDouble
    val remainingCPUTime: Double = (nbSubproblems - subproblemsDone).toDouble * meanCPUTime

    wallTimeRemaining.setText(round1000(remainingWallTime / math.pow(10, 9)) + "s")
    wallTimeElapsed.setText(round1000(elapsedWallTime / math.pow(10, 9)) + "s")
    wallTimeTotal.setText(round1000((elapsedWallTime + remainingWallTime) / math.pow(10, 9)) + "s")

    cpuTimeRemaining.setText(round1000(remainingCPUTime / math.pow(10, 9)) + "s")
    cpuTimeElapsed.setText(round1000(elapsedCPUTime / math.pow(10, 9)) + "s")
    cpuTimeTotal.setText(round1000((elapsedCPUTime + remainingCPUTime) / math.pow(10, 9)) + "s")

    cpuWallRatio.setText(round1000(elapsedCPUTime / elapsedWallTime).toString)

    totalWallTimePlotPoints.add(elapsedWallTime / math.pow(10, 9), (elapsedWallTime + remainingWallTime) / math.pow(10, 9))
    totalCPUTimePlotPoints.add(elapsedWallTime / math.pow(10, 9), (elapsedCPUTime + remainingCPUTime) / math.pow(10, 9))

    meanWallTimePlotPoints.add(elapsedWallTime / math.pow(10, 9), meanWallTime / math.pow(10, 9))
    meanCPUTimePlotPoints.add(elapsedWallTime / math.pow(10, 9), meanCPUTime / math.pow(10, 9))
    instantMeanCPUTimePlotPoints.add(elapsedWallTime / math.pow(10, 9), instantMeanTime.get)

    timeStorage += ((elapsedWallTime, remainingWallTime, elapsedCPUTime, remainingCPUTime))
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

  override def newSolution(solution: T): Unit = solutionsFound += 1


  override def endedSubproblem(spid: Int, timeTaken: Double): Unit = {
    subproblemsTime(spid) = timeTaken
    subproblemsResolving -= spid
    subproblemsDone += 1
    instantMeanTime.update(timeTaken / Math.pow(10, 9))
    subproblemsTotalCPUTime += timeTaken

    Swing.onEDT {
      subproblemCPUTimeHistogram.addObservation(timeTaken / Math.pow(10, 9))
      updateProgressBar()
      updateSubproblemStatus()
      updateTime()
      updateSolutionsFound()
    }
  }

  override def allDone(): Unit = {
    val totalWallTime: Double = getClockTime - startTime
    val totalCPUTime: Double = subproblemsTotalCPUTime

    Swing.onEDT {
      subproblemCPUTimeHistogram.reinit()

      val errorTotalWallTime = new XYSeries("Total wall time error")
      val errorTotalCPUTime = new XYSeries("Total CPU time error")
      val errorRemainingWallTime = new XYSeries("Remaining wall time error")
      val errorRemainingCPUTime = new XYSeries("Remaining CPU time error")

      val collectionError = new xy.XYSeriesCollection()
      collectionError.addSeries(errorTotalWallTime)
      collectionError.addSeries(errorTotalCPUTime)
      collectionError.addSeries(errorRemainingWallTime)
      collectionError.addSeries(errorRemainingCPUTime)

      for ((ewall, rwall, ecpu, rcpu) <- timeStorage) {
        val twall = ewall + rwall
        val tcpu = ecpu + rcpu
        val t = ewall / math.pow(10, 9)
        errorTotalWallTime.add(t, 100.0 * math.abs((totalWallTime - twall) / totalWallTime))
        errorTotalCPUTime.add(t, 100.0 * math.abs((totalCPUTime - tcpu) / totalCPUTime))
        errorRemainingWallTime.add(t, 100.0 * math.abs(((totalWallTime - ewall) - rwall) / (totalWallTime - ewall)))
        errorRemainingCPUTime.add(t, 100.0 * math.abs(((totalCPUTime - ecpu) - rcpu) / (totalCPUTime - ecpu)))
      }

      val errorChart = ChartFactory.createXYLineChart("", "Wall time", "% error",
        collectionError, PlotOrientation.VERTICAL, true, true, false)

      errorChart.setBackgroundPaint(new Color(255, 255, 255, 0))
      errorChart.setBackgroundImageAlpha(0.0f)

      val errorChartPanel: ChartPanel = new ChartPanel(errorChart)
      errorChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
      errorChartPanel.setRangeZoomable(true)

      chartPanel.add(errorChartPanel)
    }
  }
}

object SubproblemGraphicalProgressBar {
  def apply[T](nbSubproblems: Int, nbThreads: Int, windowName: String = "Progress Bar"): SubproblemGraphicalProgressBar[T] = {
    var gpb: SubproblemGraphicalProgressBar[T] = null

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
