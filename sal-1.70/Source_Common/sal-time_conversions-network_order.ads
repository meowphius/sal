--  Abstract :
--
--  GDS clocks package.
--
--  Notice
--
--  Copyright (C) 2002 - 2004 National Aeronautics and Space Administration.
--  No copyright is claimed in the United States under Title 17, U.S.
--  Code. All Foreign Rights are Reserved to the U.S. Government.
--
--  Disclaimer
--
--  This software is provided "as is" without any warranty of any kind,
--  either express, implied, or statutory, including, but not limited
--  to, any warranty that the software will conform to specifications,
--  any implied warranties of merchantability, fitness for a particular
--  purpose, and freedom from infringement, and any warranty that the
--  documentation will conform to the program, or any warranty that the
--  software will be error free.
--
--  In no event shall NASA be liable for any damages, including, but not
--  limited to direct, indirect, special or consequential damages,
--  arising out of, resulting from, or in any way connected with the
--  software or its documentation.  Whether or not based upon warranty,
--  contract, tort or otherwise, and whether or not loss was sustained
--  from, or arose out of the results of, or use of, the software,
--  documentation or services provided hereunder.
--
--  Export Control
--
--  The recipient of this software from NASA shall not export or
--  re-export directly or indirectly (including via remote access,
--  i.e. Internet) any part of this software or its documentation to any
--  country for which a validated license is required for such export or
--  re-export under the EXPORT LAWS without first obtaining such a
--  validated license.
--

pragma License (Modified_GPL);

with SAL.Network_Order.Gen_Scalar_64;
package SAL.Time_Conversions.Network_Order is new SAL.Network_Order.Gen_Scalar_64 (Time_Type);
