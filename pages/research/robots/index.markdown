---
Title: Robots
category: robots
---

## Generation 2 ## {.fullWidth}

Details coming soon.

## Generation 1 ## {.fullWidth}

Our custom robotic platform is built around a novel two-surface wood housing,
whose upper surface serves as a workspace. A 5-DOF manipulator arm is mounted
on the upper housing surface while a 1-DOF pendulum arm is mounted on the lower
housing surface.  A head containing two cameras with individually controllable
pan and tilt is mounted on the pendulum arm which allows the cameras to rotate
180 degrees around the workspace to image that space from a variety of
viewpoints.

§youtube(sB9MZD-VKwA)§

![](annotated-hand.jpg)

An arm with two independently controllable fingers is mounted on the
manipulator arm and contains a number of endogenous sensors in addition to the
exogenous head-mounted cameras: a <font color="#00da00">palm mounted
camera</font>, an <font color="#f600f7">ultrasonic range sensor</font>, a <font
color="#fe0000">computer-controllable laser pointer</font> to assist depth
estimation, and two independent <font color="#ff7e00">force sensors</font> on
each finger, one on the inside surface and one on the fingertip.

### Arm ### {.fullWidth}

The arm base is a
[standard Lynxmotion design documented on their website](http://www.lynxmotion.com/images/html/build093.htm). We
use only one servo, a Hitec HS-5995TG, and therefore we replace the ASB-13
bracket with an ASB-04B; we weld the L bracket to the base base instead of
screwing it in for a stronger joint that vibrates less. The lower (shoulder),
middle (elbow) and front (hand), servos are connected similarly, to
[the Lynxmotion SES arm](http://www.lynxmotion.com/images/html/build118.htm). We
use different servos than the SES arm; the shoulder is a Hitec HS-985MG, the
elbow is a Hitec HS-645MG, the hand and fingers are Hitec HS-5475. These servos
have been carefully chosen to minimize the amount of vibration, and to allow
the two most taxed ones to relax by having analog servos installed in those
positions. More modern Hitec digital servos can relax but the ones we had
available at the time could not. We use different link lengths from the SES arm
(AT-05 for both the forearm and upper-arm), and use a different spring
configuration (we add one spring attached to the lower back servo mount screw
and the hub screw at the top of the shoulder C bracket). The gripper is a
custom design. To the end of the wrist L bracket (part of the standard joint
configuration), we connect a C bracket, ASB-09B, on to which we mount two
multi-purpose brackets, ASB-04B. On each of these we mount one servo, Hitec
HS-5475, one offset bracket, ASB-11B, one hub, HUB-08, one tube, AT-01. We
cover the grippers using a rubber end cap, REC-06. The servos are powered by an
adjustable Mean Well SP-320-5 at 7 volts.\


| Name                                                                                                                                | Model #   | Quantity |
|:------------------------------------------------------------------------------------------------------------------------------------+----------:+---------:|
| [Aluminum &quot;C&quot; Servo Bracket with Ball Bearings Two Pack (Brushed)](http://lynxmotion.com/Product.aspx?ProductID=413)      | ASB-09B   |        1 |
| [Aluminum &quot;L&quot; Connector Bracket Two Pack (Brushed)](http://lynxmotion.com/Product.aspx?ProductID=412)                     | ASB-06    |        2 |
| [Aluminum Long &quot;C&quot; Servo Bracket with Ball Bearings Two Pack (Brushed)](http://lynxmotion.com/Product.aspx?ProductID=326) | ASB-10B   |        1 |
| [Aluminum Multi-Purpose Servo Bracket Two Pack (Brushed)](http://lynxmotion.com/Product.aspx?ProductID=411)                         | ASB-04B   |        3 |
| [Aluminum Offset Servo Bracket with Ball Bearings Two Pack (Brushed)](http://lynxmotion.com/Product.aspx?ProductID=415)             | ASB-11B   |        1 |
| [Aluminum Tubing - 3.375in](http://lynxmotion.com/Product.aspx?ProductID=409)                                                       | AT-06     |        2 |
| [Aluminum Tubing Connector Hub (pair)](http://lynxmotion.com/Product.aspx?ProductID=403)                                            | HUB-08    |        2 |
| [Hitec Spline Metal Servo Horn (Tapped)](http://lynxmotion.com/Product.aspx?ProductID=505)                                          | HMSH-02   |        6 |
| [Load Balancing Spring - 1.5in 1.58lb](http://lynxmotion.com/Product.aspx?ProductID=439)                                            | SPR-01    |        4 |
| [Metal Arm Base (no servos)](http://lynxmotion.com/Product.aspx?ProductID=449)                                                      | MAB-01    |        1 |
| [Regulated Wall Pack - 6.0vdc 2.5amp](http://lynxmotion.com/Product.aspx?ProductID=634)                                             | WP-02     |        1 |
| [Servo Attachment Hardware Bag (Single)](http://lynxmotion.com/Product.aspx?ProductID=447)                                          | HDW-02    |        6 |
| [Servo Extender Cable - 12in](http://lynxmotion.com/Product.aspx?ProductID=83)                                                      | SEA-02    |        3 |
| [Servo Extender Cable - 6in](http://lynxmotion.com/Product.aspx?ProductID=82)                                                       | SEA-01    |        3 |
| [SES Arm Wrist Rotate Upgrade](http://lynxmotion.com/Product.aspx?ProductID=590)                                                    | SESAWR-KT |        1 |
| [SSC-32 Servo Controller](http://lynxmotion.com/Product.aspx?ProductID=395)                                                         | SSC-32    |        1 |


### Head ### {.fullWidth}

The camera mount is built on top of an
[MAB-01](http://www.lynxmotion.com/images/html/build093.htm), with an aluminum
[replacement milled multipurpose bracket](misc/base-c-bracket.dwg), and a
welded (not screwed) L bracket joint. Two ASB-516B are joined and connected to
the base, at the 4th hole. At the other end, leaving two holes empty, one hub,
HUB-08, is attached, along with 3 tubes, AT-06s, connected by end-to-end
hubs. The tubes are machine pressed to fit more tightly onto the hubs so as to
reduce vibration. Similarly one more tube, AT-01 is mounted on top. One
channel, ASB-507B, is mounted on top of this assembly with one hub, HUB-08,
perpendicular to the camera mount. At each end of the top of this channel one C
bracket, ASB-09B, is mounted, bent to roughly 15-20 degrees so that the cameras
will face the playing surface.

Two Logitech QuickCam Orbits, with their base disassembled, are mounted to the
top of these brackets. The mount has one custom
[plastic connector](misc/camera-attachement.dwg), to which the USB connector
from the Logitech base is glued; the connector then is screwed into the side of
ASB-09B bracket, held in place by one screw which goes through both the custom
connector and the USB connector. The previous operation requires the removal of
several small pieces of plastic from the sides of the Logitech USB
connector. The camera then slides into the standard Logitech connector, now
attached to the mount, requiring no modifications to the camera eye itself and
allowing the cameras to be swapped. This setup also provides for optional
stereo sound recording via a microphone built-in to each Logitech camera base.

The robot is controlled via a
[Lynxmotion SSC-32 controller](http://www.lynxmotion.com/Product.aspx?productID=395),
which comes with both ample documentation and source code. Sensors are
controlled through a
[Society of Robots Axon](http://www.societyofrobots.com/axon/). Unfortunately
the Axon has too few PWM generators to replace the SSC-32.

### Housing ### {.fullWidth}

The housing is built of out two identical surfaces, connected and build to
[these specifications](misc/box.dwg), the bases are screwed in to
[these specifications](misc/base-mount.xcf).
