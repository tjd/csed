package examclock;

public class ExamStateMachine {

	private ExamState currentState;
	private Exam exam;
	
	public ExamStateMachine(Exam exam) {
		this.exam = exam;
		currentState = ExamState.running;
	}
	
	public ExamState state() {
		return currentState;
	}
	
	public ExamState changeState() {
		if (exam.secondsRemaining() <= 1) {
			currentState = ExamState.finished;
		} else if (exam.minutesRemaining() <= 1) {
			currentState = ExamState.lastMinute;
		} else if (exam.minutesRemaining() <= 10) {
			currentState = ExamState.lastTenMinutes;
		}
		return state();
	}
	
	public boolean lastTenMinutes() {
		return state().equals(ExamState.lastMinute);
	}

	public boolean lastMinute() {
		return state().equals(ExamState.lastTenMinutes);
	}
	
	public boolean finished() {
		return state().equals(ExamState.finished);
	}

	public boolean running() {
		return state().equals(ExamState.running);
	}

}
