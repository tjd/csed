package examclock;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

public class Examclock {

	private static final Color PANEL_BG_COLOR = Color.WHITE;

	private static final int MSG_FONT_SIZE = 40;

	private static final Font MSG_FONT = new Font("Times", Font.PLAIN,
			MSG_FONT_SIZE);

	private static final int TIME_ELAPSED_FONT_SIZE = 20;

	private static final Font TIME_ELAPSED_FONT = new Font("Helvetica",
			Font.BOLD, TIME_ELAPSED_FONT_SIZE);

	private static final int TIME_REMAINING_FONT_SIZE = 240;

	private static final Font TIME_REMAINING_FONT = new Font("Helvetica",
			Font.BOLD, TIME_REMAINING_FONT_SIZE);

	private static final String FRAME_NAME = "Exam Clock";

	private static final int PANEL_WIDTH = Toolkit.getDefaultToolkit()
			.getScreenSize().width;

	private static final int PANEL_HEIGHT = Toolkit.getDefaultToolkit()
			.getScreenSize().height / 2;

	public static void main(String[] args) {
		Exam exam = askUserForExamInfo();
		System.out.printf("%s", exam);

		// make the panel for the labels
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.setPreferredSize(new Dimension(PANEL_WIDTH, PANEL_HEIGHT));
		panel.setBackground(PANEL_BG_COLOR);

		// time remaining label
		JLabel timeRemaining = new JLabel();
		timeRemaining.setFont(TIME_REMAINING_FONT);
		timeRemaining.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeRemaining, BorderLayout.CENTER);

		// time elapsed label
		JLabel timeElapsed = new JLabel();
		timeElapsed.setFont(TIME_ELAPSED_FONT);
		timeElapsed.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeElapsed, BorderLayout.NORTH);

		// message label
		JLabel msg = new JLabel();
		msg.setFont(MSG_FONT);
		msg.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(msg, BorderLayout.SOUTH);

		// set up timers
		Timer clockTimer = new Timer();
		TimerTask clockUpdate = new UpdateClockTask(timeRemaining, timeElapsed,
				exam);
		clockTimer.scheduleAtFixedRate(clockUpdate, 0, 60 * 1000);

		// Timer msgTimer = new Timer();
		TimerTask advice = new AdviceGiver(msg, exam);
		clockTimer.schedule(advice, 10 * 1000, 30 * 1000);

		// prepare the frame
		JFrame frame = new JFrame(FRAME_NAME);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(panel, BorderLayout.CENTER);
		frame.pack();
		frame.setVisible(true);
	}

	public static Exam askUserForExamInfo() {
		// Scanner sc = new Scanner(System.in);
		// System.out.printf("How long is the exam in minutes? --> ");
		String inval = JOptionPane
				.showInputDialog("Enter exam length in minutes");
		int minutes = Integer.parseInt(inval);
		return new Exam(minutes);
	}
}

class AdviceGiver extends TimerTask {
	private static final int FINAL_MSG_TIME = 10;

	private static final String EXAM_OVER_MSG = "Hand in your exam!";

	private JLabel label;

	private Exam exam;

	private int nextMsg;

	private String[] msg;

	private String[] earlyMsg = {
			"Write your name and student number on all pages.",
			"Finished early? Double-check your answers!",
			"Read the questions carefully!", "Think.", "Relax.",
			"Have a question? Raise your hand!",
			"Have mercy on your marker: write clearly!" };

	private String[] finalMsg = { "Finish what you are working on!",
			"Please stay seated until the end of the exam.",
			"Is your name and student number on the exam?",
			"Double-check your answers!" };

	public AdviceGiver(JLabel label, Exam exam) {
		msg = earlyMsg;
		nextMsg = 0;
		this.label = label;
		this.exam = exam;
	}

	public void useFinalMessages() {
		if (msg != finalMsg) {
			msg = finalMsg;
			nextMsg = 0;
		}
	}

	@Override
	public void run() {
		if (exam.finished()) {
			label.setText(EXAM_OVER_MSG);
		} else {
			if (exam.minutesRemaining() <= FINAL_MSG_TIME) {
				useFinalMessages();
			}
			label.setText(msg[nextMsg]);
			nextMsg = (nextMsg + 1) % msg.length;
		}
	}
}

class UpdateClockTask extends TimerTask {
	private static final String EXAM_FINISHED_MSG = "Time's up!";

	private static final Color LAST_TEN_MINUTES_COLOR = Color.ORANGE;

	private static final Color LAST_MINUTE_COLOR = Color.RED;

	private JLabel timeRemaining;

	private JLabel timeElapsed;

	private Exam exam;

	public UpdateClockTask(JLabel timeRemaining, JLabel timeElapsed, Exam exam) {
		this.timeRemaining = timeRemaining;
		this.timeElapsed = timeElapsed;
		this.exam = exam;
	}

	@Override
	public void run() {
		final long remaining = exam.minutesRemaining();
		final long elapsed = exam.minutesElapsed();

		if (exam.lastMinute()) {
			timeRemaining.setForeground(LAST_MINUTE_COLOR);
		} else if (exam.lastTenMinutes()) {
			timeRemaining.setForeground(LAST_TEN_MINUTES_COLOR);
		}

		if (exam.finished()) {
			timeRemaining.setText(String.format(EXAM_FINISHED_MSG));
		} else {
			timeRemaining.setText(String.format("%s min", remaining));
		}
		timeElapsed.setText(String.format("%s minute%s elapsed", elapsed,
				(elapsed == 1) ? "" : "s"));
	}
}

enum ExamState {
	running, lastTenMinutes, lastMinute, finished
}

class Exam {
	private final Calendar startTime;

	private final Calendar endTime;

	private final long endTimeMillis;

	private final int durInSeconds;

	private ExamState state;

	public Exam(int mins) {
		this(0, mins);
	}

	public Exam(int hours, int mins) {
		durInSeconds = 60 * mins + 60 * 60 * hours;

		this.startTime = new GregorianCalendar();
		this.endTime = (Calendar) startTime.clone();
		this.endTime.add(Calendar.HOUR, hours);
		this.endTime.add(Calendar.MINUTE, mins);
		endTimeMillis = endTime.getTime().getTime();
		state = ExamState.running;
	}

	public String toString() {
		StringBuilder result = new StringBuilder("");
		result.append(String.format("Start time: %s\n", this.startTime
				.getTime()));
		result.append(String.format("End time: %s\n", this.endTime.getTime()));
		result.append(String.format("Duration: %ss \n", durInSeconds));
		result.append(String.format("State: %s\n", this.state));
		return result.toString();
	}

	public long getDurInMins() {
		return (long) Math.round((durInSeconds / 60.0));
	}

	public long getDurInSeconds() {
		return durInSeconds;
	}

	public Date getEndTime() {
		return endTime.getTime();
	}

	private long getCurrentTime() {
		return System.currentTimeMillis();
	}

	public long hoursRemaining() {
		return (long) Math.round(millisRemaining() / (1000.0 * 60 * 60));
	}

	public long minutesRemaining() {
		return (long) Math.round(millisRemaining() / (1000.0 * 60));
	}

	public long secondsRemaining() {
		return (long) Math.round(millisRemaining() / 1000.0);
	}

	private long millisRemaining() {
		return endTimeMillis - getCurrentTime();
	}

	public long hoursElapsed() {
		return Math.round(secondsElapsed() / (60.0 * 60));
	}

	public long minutesElapsed() {
		return Math.round(secondsElapsed() / 60.0);
	}

	public long secondsElapsed() {
		return getDurInSeconds() - secondsRemaining();
	}

	public boolean lastTenMinutes() {
		if (state != ExamState.lastTenMinutes && minutesRemaining() <= 10) {
			state = ExamState.lastTenMinutes;
		}
		return state == ExamState.lastTenMinutes;
	}

	public boolean lastMinute() {
		if (state != ExamState.lastMinute && minutesRemaining() <= 1) {
			state = ExamState.lastMinute;
		}
		return state == ExamState.lastMinute;
	}

	public boolean finished() {
		if (state != ExamState.finished && secondsRemaining() <= 1) {
			state = ExamState.finished;
		}
		return state == ExamState.finished;
	}

	public Date getStartTime() {
		return startTime.getTime();
	}

}
