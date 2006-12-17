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
	public static final String FRAME_NAME = "Exam Clock";

	public static final int PANEL_WIDTH = Toolkit.getDefaultToolkit()
			.getScreenSize().width;

	public static final int PANEL_HEIGHT = Toolkit.getDefaultToolkit()
			.getScreenSize().height / 2;

	public static final Color PANEL_BG_COLOR = Color.WHITE;

	public static final MessageFont MSG = new MessageFont("", new Font("Times",
			Font.PLAIN, 40));

	public static final MessageFont TIME_ELAPSED = new MessageFont("",
			new Font("Helvetica", Font.BOLD, 20));

	public static final MessageFont TIME_REMAINING = new MessageFont("",
			new Font("Helvetica", Font.BOLD, 240));

	public static void main(String[] args) {
		Exam exam = askUserForExamInfo();
		System.out.printf("%s", exam);

		// make the panel for the labels
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.setPreferredSize(new Dimension(PANEL_WIDTH, PANEL_HEIGHT));
		panel.setBackground(PANEL_BG_COLOR);

		// make time remaining panel
		TimePanel timeRemainingPanel = new TimePanel(exam.getHoursRemaining(),
				exam.getMinutesRemaining());

		panel.add(timeRemainingPanel);

		// time elapsed label
		JLabel timeElapsed = new JLabel();
		timeElapsed.setFont(TIME_ELAPSED);
		timeElapsed.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(timeElapsed, BorderLayout.NORTH);

		// message label
		JLabel msg = new JLabel();
		msg.setFont(MSG);
		msg.setHorizontalAlignment(SwingConstants.CENTER);
		panel.add(msg, BorderLayout.SOUTH);

		// set up timers
		Timer clockTimer = new Timer();
		TimerTask clockUpdate = new UpdateClockTask(timeRemainingPanel,
				timeElapsed, exam);
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
		String inval = JOptionPane
				.showInputDialog("Enter exam length in minutes");
		int minutes = Integer.parseInt(inval);
		return new Exam(minutes);
	}
}

class TimePanel extends JPanel {
	private MessageFont hours;

	private MessageFont minutes;

	private MessageFont hr;

	private MessageFont min;

	private MessageFont FINISHED_MSG = new MessageFont("hr ", new Font(
			"Helevetica", Font.BOLD, Examclock.TIME_REMAINING.getSize() - 40));

	private JLabel hoursLabel;

	private JLabel minutesLabel;

	private JLabel hrLabel;

	private JLabel minLabel;

	public TimePanel(long hours, long mins) {
		final long h = mins / 60;
		final long m = mins % 60;
		this.hours = new MessageFont("" + h, new Font("Helevetica", Font.BOLD,
				Examclock.TIME_REMAINING.getSize()));
		this.hr = new MessageFont("hr ", new Font("Helevetica", Font.BOLD,
				Examclock.TIME_REMAINING.getSize() - 40));
		this.minutes = new MessageFont("" + m, new Font("Helevetica",
				Font.BOLD, Examclock.TIME_REMAINING.getSize()));
		this.min = new MessageFont("min", new Font("Helevetica", Font.BOLD,
				Examclock.TIME_REMAINING.getSize() - 40));

		hoursLabel = this.hours.toLabel();
		minutesLabel = this.minutes.toLabel();
		hrLabel = this.hr.toLabel();
		minLabel = this.min.toLabel();

		add(hoursLabel);
		add(hrLabel);
		add(minutesLabel);
		add(minLabel);
		setBackground(Examclock.PANEL_BG_COLOR);
	}

	public void updateTime(long mins) {
		long h = mins / 60;
		long m = mins % 60;
		updateTime(h, m);
	}

	public void updateTime(long hours, long mins) {
		this.hoursLabel.setText("" + hours);
		this.minutesLabel.setText("" + mins);
	}

	public void setFinished() {
		this.removeAll();
		add(FINISHED_MSG.toLabel());
	}
}

// class ClockLabel extends JLabel {
// public ClockLabel(String msg) {
// this(msg, Examclock.TIME_REMAINING);
// }
//
// public ClockLabel(String msg, int size) {
// this(msg, Examclock.TIME_REMAINING, size);
// }
//
// public ClockLabel(String msg, Font f) {
// super(msg);
// setFont(f);
// }
//
// public ClockLabel(String msg, Font f, int size) {
// super(msg);
// setFont(new Font(f.getFontName(), Font.BOLD, size));
// }
// }

class MessageFont extends Font {
	private String message;

	public MessageFont(String msg, Font f) {
		this(msg, f.getName(), f.getStyle(), f.getSize());
	}

	public MessageFont(String msg, String name, int style, int size) {
		super(name, style, size);
		this.message = msg;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String msg) {
		this.message = msg;
	}

	public JLabel toLabel() {
		JLabel lbl = new JLabel(message);
		lbl.setFont(this);
		return lbl;
	}

}

class AdviceGiver extends TimerTask {
	private static final int FINAL_MSG_TIME = 10;

	private static final String EXAM_OVER_MSG = "Hand in your exam!";

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
		if (exam.isFinished()) {
			adviceLabel.setText(EXAM_OVER_MSG);
		} else {
			if (exam.getMinutesRemaining() <= FINAL_MSG_TIME) {
				useFinalMessages();
			}
			adviceLabel.setText(msg[nextMsg]);
			nextMsg = (nextMsg + 1) % msg.length;
		}
	}
}

class UpdateClockTask extends TimerTask {
	public static final String EXAM_FINISHED_MSG = "Time's up!";

	private static final Color LAST_TEN_MINUTES_COLOR = Color.ORANGE;

	private static final Color LAST_MINUTE_COLOR = Color.RED;

	private TimePanel timeRemaining;

	private JLabel timeElapsed;

	private Exam exam;

	public UpdateClockTask(TimePanel timeRemaining, JLabel timeElapsed,
			Exam exam) {
		this.timeRemaining = timeRemaining;
		this.timeElapsed = timeElapsed;
		this.exam = exam;
	}

	public static String sIf1(long n) {
		return (n == 1) ? "" : "s";
	}

	public static String minToTime(long min) {
		// min = 60*h + m
		// 0 <= m < 60
		assert min >= 0;
		long h = min / 60;
		long m = min % 60;
		if (h < 1) {
			return String.format("%s min", m);
		} else if (m == 0) {
			return String.format("%s hour%s", h, sIf1(h));
		} else {
			return String.format("%shr %smin", h, m);
		}
	}

	@Override
	public void run() {
		final long remaining = exam.getMinutesRemaining();
		final long elapsed = exam.getMinutesElapsed();

		if (exam.isLastMinute()) {
			timeRemaining.setForeground(LAST_MINUTE_COLOR);
		} else if (exam.isLastTenMinutes()) {
			timeRemaining.setForeground(LAST_TEN_MINUTES_COLOR);
		}

		if (exam.isFinished()) {
			timeRemaining.setFinished();
			// timeRemaining.setText(String.format(EXAM_FINISHED_MSG));
		} else {
			timeRemaining.updateTime(remaining);
			// timeRemaining.setText(minToTime(remaining));
		}

		timeElapsed.setText(String.format("%s minute%s elapsed", elapsed,
				sIf1(elapsed)));
	}
}

class Exam {
	enum StateEnum {
		running, lastTenMinutes, lastMinute, finished
	}

	private final Calendar startTime;

	private final Calendar endTime;

	private final long endTimeMillis;

	private final int durInSeconds;

	private StateEnum currentState;

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
		currentState = StateEnum.running;
	}

	public String toString() {
		StringBuilder result = new StringBuilder("");
		result.append(String.format("Start time: %s\n", this.startTime
				.getTime()));
		result.append(String.format("End time: %s\n", this.endTime.getTime()));
		result.append(String.format("Duration: %ss \n", durInSeconds));
		result.append(String.format("State: %s\n", this.currentState));
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

	public long getHoursRemaining() {
		return (long) Math.round(getMillisRemaining() / (1000.0 * 60 * 60));
	}

	public long getMinutesRemaining() {
		return (long) Math.round(getMillisRemaining() / (1000.0 * 60));
	}

	public long getSecondsRemaining() {
		return (long) Math.round(getMillisRemaining() / 1000.0);
	}

	private long getMillisRemaining() {
		return endTimeMillis - getCurrentTime();
	}

	public long getHoursElapsed() {
		return Math.round(getSecondsElapsed() / (60.0 * 60));
	}

	public long getMinutesElapsed() {
		return Math.round(getSecondsElapsed() / 60.0);
	}

	public long getSecondsElapsed() {
		return getDurInSeconds() - getSecondsRemaining();
	}

	public boolean isLastTenMinutes() {
		if (currentState != StateEnum.lastTenMinutes
				&& getMinutesRemaining() <= 10) {
			currentState = StateEnum.lastTenMinutes;
		}
		return currentState == StateEnum.lastTenMinutes;
	}

	public boolean isLastMinute() {
		if (currentState != StateEnum.lastMinute && getMinutesRemaining() <= 1) {
			currentState = StateEnum.lastMinute;
		}
		return currentState == StateEnum.lastMinute;
	}

	public boolean isFinished() {
		if (currentState != StateEnum.finished && getSecondsRemaining() <= 1) {
			currentState = StateEnum.finished;
		}
		return currentState == StateEnum.finished;
	}

	public Date getStartTime() {
		return startTime.getTime();
	}

}
