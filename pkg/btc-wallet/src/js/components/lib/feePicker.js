import React, { useEffect, useState, useRef } from 'react';
import {
  Box,
  Text,
  Col,
  StatelessRadioButtonField as RadioButton,
  Label,
} from '@tlon/indigo-react';

const feeLevels = {
  low: 'low',
  mid: 'mid',
  high: 'high',
};

const FeePicker = ({ feeChoices, feeSelect, feeDismiss }) => {
  const [feeSelected, setFeeSelected] = useState(feeLevels.mid);
  const [modalElement, setModalElement] = useState();
  const modalRef = useRef();

  // const clickDismiss = (e) => {
  // console.log(modalElement, e);
  // // if (modalRef && !modalRef.contains(e.target)) {
  // // feeDismiss();
  // // }
  // };

  const select = (which) => {
    setFeeSelected(which);
    feeSelect(which);
    feeDismiss();
  };

  // useEffect(() => {
  // document.addEventListener('click', (e) => clickDismiss(e));
  // setModalElement(modalRef.current);
  // console.log(modalRef.current);
  // return () => document.addEventListener('click', clickDismiss);
  // }, []);

  return (
    <Box
      // ref={modalRef}
      // onClick={() => feeDismiss()}
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
          selected={feeSelected === feeLevels.low}
          p="2"
          onChange={() => {
            select('low');
          }}
        >
          <Label fontSize="14px">
            Slow: {feeChoices.low[1]} sats/vbyte ~{feeChoices.low[0]}m
          </Label>
        </RadioButton>

        <RadioButton
          name="feeRadio"
          selected={feeSelected === feeLevels.mid}
          p="2"
          onChange={() => {
            select('mid');
          }}
        >
          <Label fontSize="14px">
            Normal: {feeChoices.mid[1]} sats/vbyte ~{feeChoices.mid[0]}m
          </Label>
        </RadioButton>

        <RadioButton
          name="feeRadio"
          selected={feeSelected === feeLevels.high}
          p="2"
          onChange={() => {
            select('high');
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
