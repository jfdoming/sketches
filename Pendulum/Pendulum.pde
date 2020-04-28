double HP = Math.PI / 2;
float fRate = 240;

//double g = 9.8;
//double r1 = 100;
//double r2 = 100;

//double x0 = 0;
//double y0 = 0;
//double x1 = 0;
//double y1 = 0;
//double x2 = 0;
//double y2 = 0;
int dragging = 0;

// mass in kg
//double m0 = 5;
//double m1 = 50;
//double m2 = 50;

//float traceDiameter = 2;
//int traceWeight = 3; // 0 < traceWeight < 256

//PGraphics fade;
boolean display = true;

DoublePendulum system;
DoublePendulumRenderer renderer;

void setup() {
  size(500, 500);
  frameRate(fRate);
  
  //fade = createGraphics(width, height);
  //fade.beginDraw();
  //fade.background(255);
  //fade.endDraw();
  
  DoublePendulumParams systemParams = new DoublePendulumParams();
  systemParams.centreX = width / 2;
  systemParams.centreY = height / 2;
  systemParams.simulationSpeed = 4.0;
  systemParams.mass1InitialAngle = Math.PI;
  systemParams.mass2InitialAngle = HP;
  systemParams.mass1InitialAngularVelocity = 0.5;
  system = new DoublePendulum(systemParams);
  
  DoublePendulumRendererParams rendererParams = new DoublePendulumRendererParams();
  renderer = new DoublePendulumRenderer(system, rendererParams);
}

void update() {
  system.update();
  switch (dragging) {
    case 1:
      system.setMass1Direction(mouseX, mouseY);
      break;
    case 2:
      system.setMass2Direction(mouseX, mouseY);
      break;
  }
}

void draw() {
  update();
  
  renderer.render();
  
  //double da1 = rk4.get(0) + HP; // transform to graphically correct
  //x1 = x0 + r1 * Math.cos(da1);
  //y1 = y0 + r1 * Math.sin(da1);
  
  //double da2 = rk4.get(1) + HP; // transform to graphically correct
  //x2 = x1 + r2 * Math.cos(da2);
  //y2 = y1 + r2 * Math.sin(da2);
  
  //if (rk4.isRunning()) {
  //  fade.beginDraw();
  //  fade.blendMode(MULTIPLY);
  //  fade.noStroke();
  //  fade.fill(255 - traceWeight);
  //  fade.ellipse((float) x2, (float) y2, traceDiameter, traceDiameter);
  //  fade.endDraw();
  //}
  
  //image(fade, 0, 0);
  
  if (display) {
    //stroke(0);
    //line((float) x0, (float) y0, (float) x1, (float) y1);
    //line((float) x1, (float) y1, (float) x2, (float) y2);
    
    //noStroke();
    //fill(0);
    //ellipse((float) x0, (float) y0, (float) m0 * 2, (float) m0 * 2);
    //ellipse((float) x1, (float) y1, (float) m1 * 2, (float) m1 * 2);
    //fill(255, 0, 0);
    //ellipse((float) x2, (float) y2, (float) m2 * 2, (float) m2 * 2);
    //fill(0);
    text("FPS: " + frameRate, 0, 10);
  }
  
}

void mousePressed() {
  if (!system.isFrozen()) {
    return;
  }
  
  double dx = mouseX - system.getMass1X();
  double dy = mouseY - system.getMass1Y();
  double m1 = system.getParams().mass1;
  if (dragging == 0 && dx * dx + dy * dy <= m1 * m1 * 4) {
    system.reset();
    dragging = 1;
  }

  dx = mouseX - system.getMass2X();
  dy = mouseY - system.getMass2Y();
  double m2 = system.getParams().mass2;
  if (dragging == 0 && dx * dx + dy * dy <= m2 * m2 * 4) {
    system.reset();
    dragging = 2;
  }
}

void mouseReleased() {
  dragging = 0;
}

boolean sp = false;

void keyPressed() {
  if (!sp && key == ' ') {
    if (!system.isFrozen()) {
      system.freeze();
    } else {
      system.thaw();
    }
    sp = true;
  }
  
  if (key == CODED && keyCode == SHIFT) {
    display = false;
    renderer.getParams().drawing = false;
  }

  if (keyCode == 83) {
    system.freeze();
    
    String path = dataPath("");
    selectOutput("Save trace", "fileSelected", new File(path));
  }
}

void fileSelected(File out) {
  if (out != null) {
    renderer.save(out);
  }
}

void keyReleased() {
  if (key == ' ') {
    sp = false;
  }
  
  if (key == CODED && keyCode == SHIFT) {
    display = true;
    renderer.getParams().drawing = true;
  }
}
