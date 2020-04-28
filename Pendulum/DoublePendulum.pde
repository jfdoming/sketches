public class DoublePendulum {
  
  private final DoublePendulumParams dpp;
  
  private RKIntegrator rk;
  private double x0 = 0;
  private double y0 = 0;
  private double x1 = 0;
  private double y1 = 0;
  private double x2 = 0;
  private double y2 = 0;
  
  public DoublePendulum(DoublePendulumParams p) {
    this.dpp = new DoublePendulumParams(p);
    
    this.x0 = dpp.centreX;
    this.y0 = dpp.centreY;
    
    // see https://www.myphysicslab.com/pendulum/double-pendulum-en.html
    this.rk = new RKIntegrator(1 / dpp.targetFrameRate, dpp.order,
      new DoubleFunction() {
        public double evaluate(double[] params) {
          return params[3];
        }
      }, new DoubleFunction() {
        public double evaluate(double[] params) {
          return params[4];
        }
      }, new DoubleFunction() {
        public double evaluate(double[] params) {
          double t1 = params[1];
          double t2 = params[2];
          double w1 = params[3];
          double w2 = params[4];
          
          double g = dpp.gravity;
          double m1 = dpp.mass1;
          double m2 = dpp.mass2;
          double r1 = dpp.length1;
          double r2 = dpp.length2;
          
          double numTerm1 = g * (2 * m1 + m2) * Math.sin(t1);
          double numTerm2 = m2 * g * Math.sin(t1 - 2 * t2);
          double numTerm3 = 2 * Math.sin(t1 - t2) * m2 * (w2 * w2 * r2 + w1 * w1 * r1 * Math.cos(t1 - t2));
          double denomTerm1 = -r1 * (2 * m1 + m2 - m2 * Math.cos(2 * t1 - 2 * t2));
          
          return (numTerm1 + numTerm2 + numTerm3) / denomTerm1;
        }
      }, new DoubleFunction() {
        public double evaluate(double[] params) {
          double t1 = params[1];
          double t2 = params[2];
          double w1 = params[3];
          double w2 = params[4];
          
          double g = dpp.gravity;
          double m1 = dpp.mass1;
          double m2 = dpp.mass2;
          double r1 = dpp.length1;
          double r2 = dpp.length2;
          
          double numCoeff = 2 * Math.sin(t1 - t2);
          double numTerm1 = w1 * w1 * r1 * (m1 + m2);
          double numTerm2 = g * (m1 + m2) * Math.cos(t1);
          double numTerm3 = w2 * w2 * r2 * m2 * Math.cos(t1 - t2);
          double denomTerm1 = r2 * (2 * m1 + m2 - m2 * Math.cos(2 * t1 - 2 * t2));
          
          return numCoeff * (numTerm1 + numTerm2 + numTerm3) / denomTerm1;
        }
      }
    );
    
    rk.setSimulationSpeed(dpp.simulationSpeed);
    rk.set(0, dpp.mass1InitialAngle);
    rk.set(1, dpp.mass2InitialAngle);
    rk.set(2, dpp.mass1InitialAngularVelocity);
    rk.set(3, dpp.mass2InitialAngularVelocity);
    rk.start(millis());
  }
  
  public void update() {
    rk.update(millis());
    
    double da1 = rk.get(0) + HP; // transform to graphically correct
    x1 = x0 + dpp.length1 * Math.cos(da1);
    y1 = y0 + dpp.length1 * Math.sin(da1);
    
    double da2 = rk.get(1) + HP; // transform to graphically correct
    x2 = x1 + dpp.length2 * Math.cos(da2);
    y2 = y1 + dpp.length2 * Math.sin(da2);
  }
  
  public boolean isFrozen() {
    return !rk.isRunning();
  }
  
  public void freeze() {
    rk.pause();
  }
  
  public void reset() {
    freeze();
    rk.set(2, 0);
    rk.set(3, 0);
  }
  
  public void thaw() {
    rk.start(millis());
  }
  
  public DoublePendulumParams getParams() {
    return dpp;
  }
  
  public double getMass1X() {
    return x1;
  }
  
  public double getMass1Y() {
    return y1;
  }
  
  public double getMass2X() {
    return x2;
  }
  
  public double getMass2Y() {
    return y2;
  }
  
  public void setMass1Direction(double x, double y) {
    rk.set(0, Math.atan2(y - y0, x - x0) - HP);
  }
  
  public void setMass2Direction(double x, double y) {
    rk.set(1, Math.atan2(y - y1, x - x1) - HP);
  }
}

public class DoublePendulumParams {
  public double centreMass;
  public double mass1;
  public double mass2;
  public double gravity;
  public double length1;
  public double length2;
  public double centreX;
  public double centreY;
  
  public double mass1InitialAngle;
  public double mass2InitialAngle;
  public double mass1InitialAngularVelocity;
  public double mass2InitialAngularVelocity;
  
  public double simulationSpeed;
  public double targetFrameRate;
  public int order;
  
  public DoublePendulumParams() {
    this.centreMass = 5;
    this.mass1 = 5;
    this.mass2 = 5;
    this.gravity = 9.807;
    this.length1 = 100;
    this.length2 = 100;
    this.centreX = 50;
    this.centreY = 50;
    
    this.mass1InitialAngle = 0;
    this.mass2InitialAngle = 0;
    this.mass1InitialAngularVelocity = 0;
    this.mass2InitialAngularVelocity = 0;
    
    this.simulationSpeed = 1.0;
    this.targetFrameRate = 240;
    this.order = 4;
  }
  
  public DoublePendulumParams(DoublePendulumParams params) {
    this.centreMass = params.centreMass;
    this.mass1 = params.mass1;
    this.mass2 = params.mass2;
    this.gravity = params.gravity;
    this.length1 = params.length1;
    this.length2 = params.length2;
    this.centreX = params.centreX;
    this.centreY = params.centreY;
    
    this.mass1InitialAngle = params.mass1InitialAngle;
    this.mass2InitialAngle = params.mass2InitialAngle;
    this.mass1InitialAngularVelocity = params.mass1InitialAngularVelocity;
    this.mass2InitialAngularVelocity = params.mass2InitialAngularVelocity;
    
    this.simulationSpeed = params.simulationSpeed;
    this.targetFrameRate = params.targetFrameRate;
    this.order = params.order;
  }
}

public class DoublePendulumRenderer {
  
  private DoublePendulumRendererParams dprp;
  private DoublePendulum system;
  
  private PGraphics traceBuffer;
  
  public DoublePendulumRenderer(DoublePendulum system, DoublePendulumRendererParams p) {
    this.system = system;
    this.dprp = p;
    
    traceBuffer = createGraphics(width, height);
    traceBuffer.beginDraw();
    traceBuffer.background(255);
    traceBuffer.endDraw();
  }
  
  public DoublePendulumRendererParams getParams() {
    return dprp;
  }
  
  public void render() {
    DoublePendulumParams systemParams = system.getParams();
    double m0 = systemParams.centreMass;
    double m1 = systemParams.mass1;
    double m2 = systemParams.mass2;
    double x0 = systemParams.centreX;
    double y0 = systemParams.centreY;
    double x1 = system.getMass1X();
    double y1 = system.getMass1Y();
    double x2 = system.getMass2X();
    double y2 = system.getMass2Y();
    
    if (!system.isFrozen()) {
      traceBuffer.beginDraw();
      traceBuffer.blendMode(MULTIPLY);
      traceBuffer.noStroke();
      traceBuffer.fill(255 - dprp.traceWeight);
      traceBuffer.ellipse((float) x2, (float) y2, dprp.traceWidth, dprp.traceWidth);
      traceBuffer.endDraw();
    }
    
    if (dprp.tracing) {
      image(traceBuffer, 0, 0);
    }
    
    if (dprp.drawing) {
      stroke(0);
      line((float) x0, (float) y0, (float) x1, (float) y1);
      line((float) x1, (float) y1, (float) x2, (float) y2);
      
      noStroke();
      fill(0);
      ellipse((float) x0, (float) y0, (float) m0 * 2, (float) m0 * 2);
      ellipse((float) x1, (float) y1, (float) m1 * 2, (float) m1 * 2);
      fill(255, 0, 0);
      ellipse((float) x2, (float) y2, (float) m2 * 2, (float) m2 * 2);
      fill(0);
    }
  }
  
  public void save(File path) {
    traceBuffer.save(path.getAbsolutePath());
  }
}

public class DoublePendulumRendererParams {
  public boolean drawing;
  public boolean tracing;
  public float traceWidth;
  public int traceWeight;
  
  public DoublePendulumRendererParams() {
    this.drawing = true;
    this.tracing = true;
    this.traceWidth = 2;
    this.traceWeight = 3;
  }
}
