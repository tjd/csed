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

	public static void main(String[] args) {
		Exam exam = askUserForExamInfo();
		System.out.printf("%s", exam);

		// make the panel for the labels
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.setPreferredSize(new Dimension(Data.PANEL_WIDTH,
				Data.PANEL_HEIGHT));
		panel.setBackground(Data.PANEL_BG_COLOR);

		// time remaining label
		JLabel timeRemaining = new JLabel();
		timeRemaining.setFont(Data.TIME_REMAINING_FONT);
		timeRemaining.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeRemaining, BorderLayout.CENTER);

		// time elapsed label
		JLabel timeElapsed = new JLabel();
		timeElapsed.setFont(Data.TIME_ELAPSED_FONT);
		timeElapsed.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeElapsed, BorderLayout.NORTH);

		// message label
		JLabel msg = new JLabel();
		msg.setFont(Data.MSG_FONT);
		msg.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(msg, BorderLayout.SOUTH);

		// prepare the frame
		JFrame frame = new JFrame(Data.FRAME_NAME);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(panel, BorderLayout.CENTER);
		frame.pack();

		// set up timers
		Timer clockTimer = new Timer();
		TimerTask clockUpdate = new UpdateClockTask(timeRemaining, timeElapsed,
				exam, frame);
		clockTimer.scheduleAtFixedRate(clockUpdate, 0, 60 * 1000);

		// Timer msgTimer = new Timer();
		TimerTask advice = new AdviceGiver(msg, exam);
		clockTimer.schedule(advice, 10 * 1000, 30 * 1000);

		// make the frame visible
		frame.setVisible(true);
	}

	public static Exam askUserForExamInfo() {
		String inval = JOptionPane
				.showInputDialog("Enter exam length in minutes");
		int minutes = Integer.parseInt(inval);
		return new Exam(minutes);
	}
}

class AdviceGiver extends TimerTask {

	private JLabel adviceLabel;

	private Exam exam;

	private int nextMsg;

	private String[] msg;

	private String[] earlyMsg = {
			"Write your name and student number on all pages.",
			"Double-check your answers!", "Read the questions carefully!",
			"Think.", "Relax.", "Have a question? Raise your hand!",
			"Have mercy on your marker: write clearly!" };

	private String[] finalMsg = { "Finish what you are working on!",
			"Please stay seated until the end of the exam.",
			"Is your name and student number on the exam?",
			"Double-check your answers!" };

	public AdviceGiver(JLabel label, Exam exam) {
		msg = earlyMsg;
		nextMsg = 0;
		this.adviceLabel = label;
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
			adviceLabel.setText(Data.EXAM_OVER_MSG);
		} else {
			if (exam.minutesRemaining() <= Data.FINAL_MSG_TIME) {
				useFinalMessages();
			}
			adviceLabel.setText(msg[nextMsg]);
			nextMsg = (nextMsg + 1) % msg.length;
		}
	}

}

class UpdateClockTask extends TimerTask {


	private JLabel timeRemaining;

	private JLabel timeElapsed;

	private Exam exam;

	private JFrame frame;

	public UpdateClockTask(JLabel timeRemaining, JLabel timeElapsed, Exam exam,
			JFrame frame) {
		this.timeRemaining = timeRemaining;
		this.timeElapsed = timeElapsed;
		this.exam = exam;
		this.frame = frame;
	}

	@Override
	public void run() {
		final long remaining = exam.minutesRemaining();
		final long elapsed = exam.minutesElapsed();

		if (exam.lastMinute()) {
			timeRemaining.setForeground(Data.LAST_MINUTE_COLOR);
		} else if (exam.lastTenMinutes()) {
			timeRemaining.setForeground(Data.LAST_TEN_MINUTES_COLOR);
		}

		if (exam.finished()) {
			timeRemaining.setText(String.format(Data.EXAM_FINISHED_MSG));
			frame.setTitle(Data.FRAME_NAME + ": Over!");
			// "Over!" is printed instead of Data.EXAM_FINISHED_MSG since it is
			// shorter and so more likely to fit on the window label when minimized
		} else {
			timeRemaining.setText(String.format("%s min", remaining));
			frame.setTitle(Data.FRAME_NAME + " (" + remaining + " min)");
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

class Data {

	public static final String FRAME_NAME = "Exam Clock";

	public static final int MSG_FONT_SIZE = 40;

	public static final Font MSG_FONT = new Font("Times", Font.PLAIN,
			MSG_FONT_SIZE);

	public static final Color PANEL_BG_COLOR = Color.WHITE;

	public static final int TIME_ELAPSED_FONT_SIZE = 20;

	public static final Font TIME_ELAPSED_FONT = new Font("Helvetica",
			Font.BOLD, TIME_ELAPSED_FONT_SIZE);

	public static final int TIME_REMAINING_FONT_SIZE = 240;

	public static final Font TIME_REMAINING_FONT = new Font("Helvetica",
			Font.BOLD, TIME_REMAINING_FONT_SIZE);

	public static final int PANEL_WIDTH = Toolkit.getDefaultToolkit()
			.getScreenSize().width;

	public static final int PANEL_HEIGHT = Toolkit.getDefaultToolkit()
			.getScreenSize().height / 2;

	public static final int FINAL_MSG_TIME = 10;

	public static final String EXAM_OVER_MSG = "Hand in your exam!";
	
	public static final String EXAM_FINISHED_MSG = "Time's up!";

	public static final Color LAST_TEN_MINUTES_COLOR = Color.ORANGE;

	public static final Color LAST_MINUTE_COLOR = Color.RED;
}