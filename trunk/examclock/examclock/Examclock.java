package examclock;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Scanner;
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
		panel.setPreferredSize(new Dimension(Constant.PANEL_WIDTH,
				Constant.PANEL_HEIGHT));
		panel.setBackground(Constant.PANEL_BG_COLOR);

		// time remaining label
		JLabel timeRemaining = new JLabel();
		timeRemaining.setFont(Constant.TIME_REMAINING_FONT);
		timeRemaining.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeRemaining, BorderLayout.CENTER);

		// time elapsed label
		JLabel timeElapsed = new JLabel();
		timeElapsed.setFont(Constant.TIME_ELAPSED_FONT);
		timeElapsed.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeElapsed, BorderLayout.NORTH);

		// message label
		JLabel msg = new JLabel();
		msg.setFont(Constant.MSG_FONT);
		msg.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(msg, BorderLayout.SOUTH);

		// prepare the frame
		JFrame frame = new JFrame(Constant.FRAME_NAME);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(panel, BorderLayout.CENTER);
		frame.pack();

		// set up timers
		Timer clockTimer = new Timer();
		TimerTask clockUpdate = new UpdateClockTask(timeRemaining, timeElapsed,
				exam, frame);
		clockTimer.scheduleAtFixedRate(clockUpdate, 0, 60 * 1000);

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

//	private String[] msg;

	private ArrayList<String> msg;
	
	private String[] defaultEarlyMessages = {
			"Write your name and student number on all pages.",
			"Double-check your answers!", "Read the questions carefully!",
			"Think.", "Relax.", "Have a question? Raise your hand!",
			"Have mercy on your marker: write clearly!" };

	private String[] defaultFinalMessages = {
			"Finish what you are working on!",
			"Please stay seated until the end of the exam.",
			"Is your name and student number on the exam?",
			"Double-check your answers!" };

	private ArrayList<String> early;

	private ArrayList<String> late;

	private void initializeMessages() {
		// System.out.println("cwd: " + System.getProperty("user.dir"));
		early = new ArrayList<String>();
		late = new ArrayList<String>();
		try {
			Scanner sc = new Scanner(new File("earlyMessages.txt"));
			while (sc.hasNextLine()) {
				early.add(sc.nextLine());
			}
		} catch (FileNotFoundException e) {
			System.out.println("using default early messages");
			for (String s : defaultEarlyMessages) {
				early.add(s);
			}
		}

		try {
			Scanner sc = new Scanner(new File("lateMessages.txt"));
			while (sc.hasNextLine()) {
				late.add(sc.nextLine());
			}
		} catch (FileNotFoundException e) {
			System.out.println("using default late messages");
			for (String s : defaultFinalMessages) {
				late.add(s);
			}
		}
		msg = early;
	}

	public AdviceGiver(JLabel label, Exam exam) {
		msg = early;
		nextMsg = 0;
		this.adviceLabel = label;
		this.exam = exam;
		initializeMessages();
	}

	public void useFinalMessages() {
		if (msg != late) {
			msg = late;
			nextMsg = 0;
		}
	}

	@Override
	public void run() {
		if (exam.finished()) {
			adviceLabel.setText(Constant.EXAM_OVER_MSG);
		} else {
			if (exam.minutesRemaining() <= Constant.FINAL_MSG_TIME) {
				useFinalMessages();
			}
			adviceLabel.setText(msg.get(nextMsg));
			nextMsg = (nextMsg + 1) % msg.size();
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
			timeRemaining.setForeground(Constant.LAST_MINUTE_COLOR);
		} else if (exam.lastTenMinutes()) {
			timeRemaining.setForeground(Constant.LAST_TEN_MINUTES_COLOR);
		}

		if (exam.finished()) {
			timeRemaining.setText(String.format(Constant.EXAM_FINISHED_MSG));
			frame
					.setTitle(Constant.FRAME_NAME + ": "
							+ Constant.TITLE_DONE_MSG);
		} else {
			timeRemaining.setText(String.format("%s min", remaining));
			frame.setTitle(Constant.FRAME_NAME + " (" + remaining + " min)");
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

class Constant {

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

	public static final String TITLE_DONE_MSG = "Done!";

	public static final Color LAST_TEN_MINUTES_COLOR = Color.ORANGE;

	public static final Color LAST_MINUTE_COLOR = Color.RED;
}