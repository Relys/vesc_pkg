# FLOAT ACESSORIES PACKAGE

A VESC Express package for controlling LEDs, BMS and Pubmote.

<H2>Support Future Work</H2>

Support me on Patreon: <a href='https://patreon.com/SylerTheCreator'>https://patreon.com/SylerTheCreator</a>

Buy me a coffee: <a href='https://venmo.com/sylerclayton'>https://venmo.com/sylerclayton</a>

<H2>CREDITS</H2>

Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools (avaspark), auden_builds (pubmote)
Contributors: shambler01
gr33tz: outlandnish, exphat, datboig42069
Beta Testers: Pickles

My Blog: <a href='https://sylerclayton.com'>https://sylerclayton.com</a>

<H2>RELEASE NOTES</H2>

<ul>
  <li>Memory optimization</li>
  <li>Setting save fix</li>
  <li>Motor Config fix</li>
  <li>SD card logging support</li>
  <li>Mall Grab short press LED on/off. Long press highbeams</li>
  <li>Different default pins for Avaspark RGB S3 than C3</li>
  <li>Option to disable updates for front/rear LED bars (white/red hardcoded) while motor is running to prevent flicker on PCBs prone to EMF</li>
  <li>Pulse pattern while charging</li>
  <li>Overhaul of LED patterns to use time instead of indexes (fixes Knight Rider and makes animation smoother)</li>
  <li>Dynamic way of adding new settings to EEPROM. No more resetting config while upgrading to new version with new params</li>
  <li>Support for battery cell type-specific discharge curves for battery meter pattern (stock BMS will also use now)</li>
  <li>Add handtest and connecting LED patterns</li>
  <li>Fix for GTFO strips</li>
  <li>Humidity Sensor Support</li>
  <li>Support for future refloat humidity pushback and alert</li>
</ul>

<H3>BUILD INFO</H3>

Version 3.5.18

<ul>
  <li>Amber blinker (STVO §99: 1.5 Hz, 50% duty, #FF9900) — left/right overlay on front and rear strips</li>
  <li>Pubmote blinker: X button single click = left, double click = right; auto-off after 4 flashes</li>
  <li>Manual blinker override buttons in VESC Tool Control tab</li>
  <li>Auto-blinker: triggers from roll angle while riding, configurable threshold (3–30°, default 10°), with hysteresis; enable in Advance tab</li>
  <li>Swap left/right blinker option applies to all blinker sources</li>
  <li>Horn: configurable frequency (Hz), amplitude (amps), duration (s) in Advance tab; entire sequence spawned on ESC for consistent timing</li>
  <li>Pubmote horn: Z button hold &gt; 0.8 s; set-remote-state suppressed during horn window</li>
  <li>Fix pubmote auth: element-wise MAC comparison (eq type-mismatch was always failing)</li>
  <li>Fix auto-blinker angle: uses roll (side-to-side lean) not pitch</li>
  <li>Cooperative CAN scan (can-ping loop, non-blocking) and image-save for fast boot on FW 6.6+</li>
</ul>

Source code can be found here:  <a href='https://github.com/relys/vesc%5Fpkg'>https://github.com/relys/vesc_pkg</a>