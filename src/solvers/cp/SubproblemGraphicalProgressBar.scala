package solvers.cp

import java.awt._
import java.util.TimerTask
import javax.swing._

import misc.TimeHelper._
import misc.{ExponentialMovingAverage, FixedBinsHistogramDataset, SimpleMovingAverage}
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

  private var startTime = 0.0
  def start(): Unit = startTime = getClockTime

  private val timeStorage = new ArrayBuffer[(Double, Double, Double, Double)]
  private var solutionsFound = 0

  private var lastElapsedWallTime = 0.0
  private var lastRemainingWallTime = 0.0
  private var lastElapsedCPUTime = 0.0
  private var lastRemainingCPUTime = 0.0

  private var lastBound: Option[Int] = None

  //subproblemsTime contains:
  // - for done subproblems: the time taken to solve them (cpu)
  // - for not started subproblems: 0.0
  // - for started but not yet solved subproblems: the solve start time (clock)
  private val subproblemsTime = Array.tabulate(nbSubproblems)(i => 0.0)
  private var subproblemsDone = 0
  private val subproblemsResolving = new mutable.HashSet[Int]
  private val instantMeanTime = new SimpleMovingAverage(10)
  private val exponentialMeanTime = new ExponentialMovingAverage(0.05) //TODO: value
  private var subproblemsTotalCPUTime = 0.0

  /*
    CHARTS
   */
  private val totalWallTimePlotPoints = new XYSeries("Wall")
  private val totalCPUTimePlotPoints = new XYSeries("CPU")
  private val totalCPUTimeExpPlotPoints = new XYSeries("CPU (exp average)")

  private val meanWallTimePlotPoints = new XYSeries("Mean (Wall)")
  private val meanCPUTimePlotPoints = new XYSeries("Mean (CPU)")
  private val instantMeanCPUTimePlotPoints = new XYSeries("Simple moving mean (size 10) (CPU)")
  private val exponentialMeanCPUTimePlotPoints = new XYSeries("Exp moving mean (0.1) (CPU)")

  private val boundPlotPoints = new XYSeries("")

  private val subproblemCPUTimeHistogram = new FixedBinsHistogramDataset("CPU", 50)

  private val collectionTotal = new xy.XYSeriesCollection()
  private val collectionMean = new xy.XYSeriesCollection()
  private val collectionBound = new xy.XYSeriesCollection()
  collectionTotal.addSeries(totalWallTimePlotPoints)
  collectionTotal.addSeries(totalCPUTimePlotPoints)
  collectionTotal.addSeries(totalCPUTimeExpPlotPoints)
  
  collectionMean.addSeries(meanWallTimePlotPoints)
  collectionMean.addSeries(meanCPUTimePlotPoints)
  collectionMean.addSeries(instantMeanCPUTimePlotPoints)
  collectionMean.addSeries(exponentialMeanCPUTimePlotPoints)

  collectionBound.addSeries(boundPlotPoints)

  private val totalTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Total time estimated",
    collectionTotal, PlotOrientation.VERTICAL, true, true, false)
  private val meanTimeChart = ChartFactory.createXYLineChart("", "Wall time", "Time taken per subproblem",
    collectionMean, PlotOrientation.VERTICAL, true, true, false)
  private val boundChart = ChartFactory.createXYLineChart("", "Wall time", "Bound value",
    collectionBound, PlotOrientation.VERTICAL, true, true, false)
  private val subproblemCPUTimeChart = ChartFactory.createHistogram("", "CPU Time", "#",
    subproblemCPUTimeHistogram.getRealDataset, PlotOrientation.VERTICAL, true, true, false)

  totalTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  totalTimeChart.setBackgroundImageAlpha(0.0f)
  meanTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  meanTimeChart.setBackgroundImageAlpha(0.0f)
  boundChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  boundChart.setBackgroundImageAlpha(0.0f)
  subproblemCPUTimeChart.setBackgroundPaint(new Color(255, 255, 255, 0))
  subproblemCPUTimeChart.setBackgroundImageAlpha(0.0f)

  private val totalTimeChartPanel: ChartPanel = new ChartPanel(totalTimeChart)
  totalTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val meanTimeChartPanel: ChartPanel = new ChartPanel(meanTimeChart)
  meanTimeChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
  private val boundChartPanel: ChartPanel = new ChartPanel(boundChart)
  boundChartPanel.setPreferredSize(new java.awt.Dimension(560, 367))
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

  private val chartPanel = new JPanel(new GridLayout(2, 3))
  chartPanel.add(totalTimeChartPanel)
  chartPanel.add(meanTimeChartPanel)
  chartPanel.add(boundChartPanel)
  chartPanel.add(subproblemCPUTimeChartPanel)

  add(progressBar)
  add(chartPanel)
  add(statPanel)
  add(timePanel)

  setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20))

  val timer = new java.util.Timer
  timer.scheduleAtFixedRate(new TimerTask() {
    override def run() = {
      Swing.onEDTWait {
        updateOnEDT()
      }
    }
  }, 0, 100)


  private def updateOnEDT(): Unit = {
    progressBar.setValue(subproblemsDone)
    subproblemStatus.setText("" + subproblemsDone + "/" + nbSubproblems)
    solutionsFoundLabel.setText(solutionsFound.toString)

    wallTimeRemaining.setText(round1000(lastRemainingWallTime / math.pow(10, 9)) + "s")
    wallTimeElapsed.setText(round1000(lastElapsedWallTime / math.pow(10, 9)) + "s")
    wallTimeTotal.setText(round1000((lastElapsedWallTime + lastRemainingWallTime) / math.pow(10, 9)) + "s")

    cpuTimeRemaining.setText(round1000(lastRemainingCPUTime / math.pow(10, 9)) + "s")
    cpuTimeElapsed.setText(round1000(lastElapsedCPUTime / math.pow(10, 9)) + "s")
    cpuTimeTotal.setText(round1000((lastElapsedCPUTime + lastRemainingCPUTime) / math.pow(10, 9)) + "s")

    cpuWallRatio.setText(round1000(lastElapsedCPUTime / lastElapsedWallTime).toString)

    totalWallTimePlotPoints.fireSeriesChanged()
    totalCPUTimePlotPoints.fireSeriesChanged()
    totalCPUTimeExpPlotPoints.fireSeriesChanged()
    meanWallTimePlotPoints.fireSeriesChanged()
    meanCPUTimePlotPoints.fireSeriesChanged()
    instantMeanCPUTimePlotPoints.fireSeriesChanged()
    exponentialMeanCPUTimePlotPoints.fireSeriesChanged()
  }

  private def round1000(v: Double): Double = (v * 1000).toInt.toDouble / 1000

  private def updateTime(): Unit = {
    lastElapsedWallTime = getClockTime - startTime
    val meanWallTime = lastElapsedWallTime / subproblemsDone.toDouble
    lastRemainingWallTime = (nbSubproblems - subproblemsDone).toDouble * meanWallTime

    lastElapsedCPUTime = subproblemsTotalCPUTime
    val meanCPUTime: Double = subproblemsTotalCPUTime / subproblemsDone.toDouble
    lastRemainingCPUTime = (nbSubproblems - subproblemsDone).toDouble * meanCPUTime
    val remainingCPUTimeExp = (nbSubproblems - subproblemsDone).toDouble * exponentialMeanTime.get

    totalWallTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedWallTime + lastRemainingWallTime) / math.pow(10, 9), false)
    totalCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedCPUTime + lastRemainingCPUTime) / math.pow(10, 9), false)
    totalCPUTimeExpPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), (lastElapsedCPUTime + remainingCPUTimeExp) / math.pow(10, 9), false)

    meanWallTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), meanWallTime / math.pow(10, 9), false)
    meanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), meanCPUTime / math.pow(10, 9), false)
    instantMeanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), instantMeanTime.get / math.pow(10, 9), false)
    exponentialMeanCPUTimePlotPoints.add(lastElapsedWallTime / math.pow(10, 9), exponentialMeanTime.get / math.pow(10, 9), false)

    lastBound match {
      case Some(b) => boundPlotPoints.add(lastElapsedWallTime / math.pow(10, 9), b)
      case None =>
    }

    timeStorage += ((lastElapsedWallTime, lastRemainingWallTime, lastElapsedCPUTime, lastRemainingCPUTime))
  }

  override def startedSubproblem(spid: Int): Unit = {
    subproblemsTime(spid) = getClockTime
    subproblemsResolving += spid
  }

  override def newSolution(solution: T): Unit = solutionsFound += 1


  override def endedSubproblem(spid: Int, timeTaken: Double, currentBound: Option[Int]): Unit = {
    subproblemsTime(spid) = timeTaken
    subproblemsResolving -= spid
    subproblemsDone += 1
    instantMeanTime.update(timeTaken)
    exponentialMeanTime.update(timeTaken)
    subproblemsTotalCPUTime += timeTaken
    lastBound = currentBound
    updateTime()

    Swing.onEDT {
      subproblemCPUTimeHistogram.addObservation(timeTaken / Math.pow(10, 9))
    }
  }

  override def allDone(): Unit = {
    val totalWallTime: Double = getClockTime - startTime
    val totalCPUTime: Double = subproblemsTotalCPUTime

    Swing.onEDTWait {
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
