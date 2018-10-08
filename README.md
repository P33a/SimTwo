### SimTwo - A Realistic Simulator for Robotics.

SimTwo is a realistic simulator where several types of robots can be implemented.

- Wheeled Mobile Robots with different configurations
  - Differential
  - With omnidirectional wheels

- Manipulators

- Legged Mobile Robots
  - Biped
  - Quadruped
  - Hexapods

- Lighter than air vehicles with propellers for propulsion.
- Underwater vehicles with thrusters.

Any type of robot that can be described as a mixture of rigid bodies connected by, optionally powered, hinge or prismatic joints. Classical or omnidirectional wheels and also propellers can be used to make the robots move.

A realistic environment is achieved by decomposing the robot in a system of rigid bodies, hinge or prismatic joints, optionally actuated by electric motors.

Certain  joints: hinge, universal and slider (prismatic) can be associated with a system of drive + motor and encoder.

The drive + motor system may consist of a DC motor, optionally with a gearbox and a controller. Several types of controllers can be used: a PID controller applied to the position or velocity signals or a state feedback controller.

The DC motor model contains several nonlinear elements such as voltage saturation, current limitation and Coulomb Friction.

In addition to the low level control system, SimTwo also offers the possibility to provide the reference signals to these controllers from a higher level controller implemented by the user. This controller can be implemented using:

- A scripting language, editable and compilable within SimTwo.
- A remote program that communicates via UDP packets or over the serial port.
