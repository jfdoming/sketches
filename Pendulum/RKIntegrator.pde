/**
 * This class implements a class of algorithms for numerical approximation
 * of solutions to systems of differential equations known as the Runge-Kutta
 * Methods. It allows the user to specify any number of functions in terms of
 * any variables used (including time), which represent the FIRST derivatives
 * of some other function, and, over the course of repeated computation,
 * will generate an approximation of the values of the original functions.
 *
 * WARNING: THIS IMPLEMENTATION BEHAVES CORRECTLY FOR THE RK4 METHOD ONLY.
 * IMPLEMENTATIONS OF OTHER METHODS MAY BE PROVIDED AT A LATER TIME.
 */
public class RKIntegrator {
  
  private double simulationSpeed;
  private double stepTarget;
  private int count;
  private double[] state;
  private DoubleFunction[] functions;
  
  private long startTime = -1;
  private long lastTime = -1;
  private boolean running = false;
  private boolean initialized = false;
  
  public RKIntegrator(double stepTarget, DoubleFunction... functions) {
    this(stepTarget, 4, functions);
  }
  
  public RKIntegrator(double stepTarget, int count, DoubleFunction... functions) {
    this.stepTarget = stepTarget;
    this.count = count;
    this.state = new double[functions.length];
    this.functions = functions;
    this.simulationSpeed = 1.0;
  }
  
  public void set(int index, double value) {
    state[index] = value;
  }
  
  public double get(int index) {
    return state[index];
  }
  
  public void setSimulationSpeed(double speed) {
    this.simulationSpeed = speed;
  }
  
  public long getStartTime() {
    return startTime;
  }
  
  public boolean isRunning() {
    return running;
  }
  
  public void start(long startTime) {
    if (running) {
      return;
    }
    
    this.startTime = startTime;
    lastTime = startTime;
    running = true;
  }
  
  public void pause() {
    running = false;
    initialized = false;
  }
  
  public void update(long currentTime) {
    if (!running) {
      return;
    }
    
    long t = currentTime - startTime;
    double h;
    if (initialized) {
      h = simulationSpeed * (currentTime - lastTime) / 1000.0;
    } else {
      h = stepTarget;
      initialized = true;
    }
    lastTime = currentTime;
    
    double[][] results = new double[count][functions.length];
    
    double factor = 0.5;
    for (int x = 0; x < count; x++) {
      if (x >= count - 1) {
        factor = 1;
      }
      
      for (int i = 0; i < functions.length; i++) {
        double[] input = new double[functions.length + 1];
        input[0] = t + (x > 0 ? (factor * h) : 0);
        for (int j = 0; j < state.length; j++) {
          double adjustment = (x > 0 ? (factor * results[x - 1][j]) : 0);
          input[j + 1] = state[j] + adjustment;
        }
        results[x][i] = h * functions[i].evaluate(input);
      }
    }
    
    for (int i = 0; i < functions.length; i++) {
      double sum = 0;
      for (int j = 0; j < count; j++) {
        if (j > 0 && j < count - 1) {
          sum += 2 * results[j][i];
        } else {
          sum += results[j][i];
        }
      }
      
      state[i] += sum / 6;
    }
  }
  
}