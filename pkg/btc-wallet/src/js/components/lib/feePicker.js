import React from 'react';
import {
  Box,
  Text,
  Col,
  StatelessRadioButtonField as RadioButton,
  Label,
} from '@tlon/indigo-react';
import { feeLevels } from './send';

const FeePicker = ({ feeChoices, feeValue, setFeeValue, feeDismiss }) => {
  const select = (which) => {
    setFeeValue(feeLevels[which]);
    feeDismiss();
  };

  return (
    <Box
      position="absolute"
      p={4}
      border="1px solid green"
      zIndex={10}
      backgroundColor="white"
      borderRadius={3}
    >
      <Text fontSize={1} color="black" fontWeight="bold" mb={4}>
        Transaction Speed
      </Text>
      <Col mt={4}>
        <RadioButton
          name="feeRadio"
          selected={feeValue === feeLevels.low}
          p="2"
          onChange={() => {
            select(feeLevels.low);
          }}
        >
          <Label fontSize="14px">
            Slow: {feeChoices.low[1]} sats/vbyte ~{feeChoices.low[0]}m
          </Label>
        </RadioButton>

        <RadioButton
          name="feeRadio"
          selected={feeValue === feeLevels.mid}
          p="2"
          onChange={() => {
            select(feeLevels.low);
          }}
        >
          <Label fontSize="14px">
            Normal: {feeChoices.mid[1]} sats/vbyte ~{feeChoices.mid[0]}m
          </Label>
        </RadioButton>

        <RadioButton
          name="feeRadio"
          selected={feeValue === feeLevels.high}
          p="2"
          onChange={() => {
            select(feeLevels.high);
          }}
        >
          <Label fontSize="14px">
            Fast: {feeChoices.high[1]} sats/vbyte ~{feeChoices.high[0]}m
          </Label>
        </RadioButton>
      </Col>
    </Box>
  );
};

export default FeePicker;
