import React from 'react';
import {
  Box,
  Text,
  Col,
  StatelessRadioButtonField as RadioButton,
  Label,
} from '@tlon/indigo-react';
import { FeeChoices, feeLevels } from './Send';

type Props = {
  feeChoices: FeeChoices;
  feeValue: number;
  setFeeValue: React.Dispatch<feeLevels>;
  feeDismiss: () => void;
};

const FeePicker: React.FC<Props> = ({
  feeChoices,
  feeValue,
  setFeeValue,
  feeDismiss,
}) => (
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
          setFeeValue(feeLevels.low);
          feeDismiss();
        }}
      >
        <Label fontSize="14px">
          Slow: {feeChoices[feeLevels.low][1]} sats/vbyte ~
          {feeChoices[feeLevels.low][0]}m
        </Label>
      </RadioButton>

      <RadioButton
        name="feeRadio"
        selected={feeValue === feeLevels.mid}
        p="2"
        onChange={() => {
          setFeeValue(feeLevels.mid);
          feeDismiss();
        }}
      >
        <Label fontSize="14px">
          Normal: {feeChoices[feeLevels.mid][1]} sats/vbyte ~
          {feeChoices[feeLevels.mid][0]}m
        </Label>
      </RadioButton>

      <RadioButton
        name="feeRadio"
        selected={feeValue === feeLevels.high}
        p="2"
        onChange={() => {
          setFeeValue(feeLevels.high);
          feeDismiss();
        }}
      >
        <Label fontSize="14px">
          Fast: {feeChoices[feeLevels.high][1]} sats/vbyte ~
          {feeChoices[feeLevels.high][0]}m
        </Label>
      </RadioButton>
    </Col>
  </Box>
);

export default FeePicker;
