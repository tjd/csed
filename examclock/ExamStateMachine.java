package examclock;

public class ExamStateMachine {

	private Exam.StateEnum currentState;
	private Exam exam;
	
	public ExamStateMachine(Exam exam) {
		this.exam = exam;
		currentState = Exam.StateEnum.running;
	}
	
	public Exam.StateEnum state() {
		return currentState;
	}
	
	public Exam.StateEnum changeState() {
		if (exam.getSecondsRemaining() <= 1) {
			currentState = Exam.StateEnum.finished;
		} else if (exam.getMinutesRemaining() <= 1) {
			currentState = Exam.StateEnum.lastMinute;
		} else if (exam.getMinutesRemaining() <= 10) {
			currentState = Exam.StateEnum.lastTenMinutes;
		}
		return state();
	}
	
	public boolean lastTenMinutes() {
		return state().equals(Exam.StateEnum.lastMinute);
	}

	public boolean lastMinute() {
		return state().equals(Exam.StateEnum.lastTenMinutes);
	}
	
	public boolean finished() {
		return state().equals(Exam.StateEnum.finished);
	}

	public boolean running() {
		return state().equals(Exam.StateEnum.running);
	}

}
