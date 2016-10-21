package Heart is

-- This package provides an interface to a patient's heart

   type Reading is range -10 .. +10;  -- Valid heart readings

   SENSE_ERROR : exception;  -- Raised by faulty heart data

   ----------------------------------------------------------------------------
   procedure Initialize (Patient_Name : in String);
   -- Downloads the software into the device
   --
   -- Preconditions  : Initialize was not already called
   --
   -- Postconditions : Software is downloaded in the Patient's device

   ----------------------------------------------------------------------------
   function Sense return Reading;
   -- Samples the muscle activity of the patient's heart

   -- Preconditions  : The device was installed by a call to Initialize
   --
   -- Postconditions : Returns a reading of the current heart muscle activity
   --
   -- Exceptions     : SENSE_ERROR  - raised if the heart sensing equipment
   --                                 provided faulty data

   ----------------------------------------------------------------------------
   procedure Shock;
   -- Attempts to restore normal heart rhythm

   -- Preconditions  : The device was installed by a call to Initialize
   --                  The patients heart is arrested or fibrillating
   --
   -- Postconditions : An electrical shock is applied to the patient's heart

   ----------------------------------------------------------------------------
   function Insured return Boolean;
   -- Determines whether or not a patient has insurance to cover the heart monitor
   --
   -- Preconditions  : The device was installed by a call to Initialize
   --
   -- Postconditions : Returns True if the Patient has insurance coverage
   --                  for at least one more set of heart readings.
   --                  Returns False if the Patient has no insurance coverage  


end Heart;
