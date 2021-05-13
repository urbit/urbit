import {
  BaseLabel, Box,

  Col, Icon,

  Label, Row
} from '@tlon/indigo-react';
import { useField } from 'formik';
import React, { useCallback, useMemo } from 'react';
import styled from 'styled-components';

type IconRadioProps = Parameters<typeof Row>[0] & {
  id: string;
  icon: string;
  name: string;
  disabled?: boolean;
  caption?: string;
  label: string;
};

// Hide this input completely
const HiddenInput = styled.input`
  position: absolute;
  opacity: 0;
  height: 0;
  width: 0;
  margin: 0px;
`;

type IconIndicatorProps = Parameters<typeof Box> & {
  disabled?: boolean;
  selected?: boolean;
  hasError?: boolean;
};

// stolen from indigo
// TODO: indigo should probably export this
const indicator = {
  state: {
    on: {
      // "*": { fill: "white" },
      backgroundColor: 'blue',
      borderColor: 'blue'
    },
    off: {
      // "*": { fill: "transparent" },
      backgroundColor: 'white',
      borderColor: 'lightGray'
    },
    onError: {
      // "*": { fill: "white" },
      backgroundColor: 'red',
      borderColor: 'red'
    },
    offError: {
      // "*": { fill: "transparent" },
      backgroundColor: 'washedRed',
      borderColor: 'red'
    },
    offDisabled: {
      // "*": { fill: "transparent" },
      backgroundColor: 'washedGray',
      borderColor: 'lightGray'
    },
    onDisabled: {
      // "*": { fill: "lightGray" },
      backgroundColor: 'washedGray',
      borderColor: 'lightGray'
    }
  }
};

const IconIndicator = ({ disabled, selected, hasError, children, ...rest }) => {
  const style = useMemo(() => {
    if (selected && disabled)
return indicator.state.onDisabled;
    if (selected && hasError)
return indicator.state.onError;
    if (selected)
return indicator.state.on;
    if (disabled)
return indicator.state.offDisabled;
    if (hasError)
return indicator.state.offError;
    return indicator.state.off;
  }, [selected, disabled, hasError]);

  return (
    <Box borderRadius={1} border={1} {...rest} {...style}>
      {children}
    </Box>
  );
};

export function IconRadio(props: IconRadioProps) {
  const { id, icon, disabled, caption, label, name, ...rest } = props;
  const [field, meta, { setTouched }] = useField({
    name,
    id,
    value: id,
    type: 'radio'
  });

  const onChange = useCallback(
    (e: React.ChangeEvent) => {
      setTouched(true);
      field.onChange(e);
    },
    [field.onChange, setTouched]
  );

  return (
    <Row {...rest} className='icon-radio'>
      <BaseLabel
        htmlFor={id}
        display="flex"
        flexDirection="row"
        cursor="pointer"
      >
        <IconIndicator
          hasError={meta.touched && meta.error !== undefined}
          selected={field.checked}
          disabled={disabled}
          mr={2}
        >
          <Icon
            m={2}
            color={field.checked ? 'white' : 'black'}
            icon={icon as any}
          />
        </IconIndicator>
        <Col justifyContent="space-around">
          <Label color={field.checked ? 'blue' : 'black'}>{label}</Label>
          {caption ? (
            <Label gray mt={2}>
              {caption}
            </Label>
          ) : null}
          <HiddenInput
            {...field}
            onChange={onChange}
            value={id}
            name={name}
            id={id}
            disabled={disabled}
            type="radio"
          />
        </Col>
      </BaseLabel>
    </Row>
  );
}
