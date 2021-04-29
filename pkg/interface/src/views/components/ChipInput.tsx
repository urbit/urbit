import React, {
  useCallback,
  useState,
  ReactNode,
  useEffect,
  useRef,
  ReactElement
} from 'react';
import { useField } from 'formik';
import Mousetrap from 'mousetrap';

import {
  Label,
  Row,
  Col,
  StatelessTextInput as Input,
  ErrorLabel
} from '@tlon/indigo-react';


function Chip(props: { children: ReactNode }): ReactElement {
  return (
    <Row
      alignItems="center"
      height="24px"
      borderRadius="1"
      my="1"
      p="1"
      bg="blue"
      color="white"
    >
      {props.children}
    </Row>
  );
}

interface ChipInputProps {
  id: string;
  label: string;
  caption?: string;
  placeholder: string;
  breakOnSpace?: boolean;
}

export function ChipInput(props: ChipInputProps): ReactElement {
  const { id, label, caption, placeholder } = props;
  const [{ onBlur, value }, meta, { setValue }] = useField<string[]>(
    id
  );
  const [newChip, setNextChip] = useState('');
  const onChange = useCallback(
    (e: any) => {
      setNextChip(e.target.value);
    },
    [setValue]
  );

  const addNewChip = useCallback(() => {
    setValue([...value, newChip]);
    setNextChip('');
  }, [setValue, value, newChip, setNextChip]);

  const removeLastChip = useCallback(() => {
    setValue(value.slice(0, value.length - 1));
  }, [value, setValue]);

  const inputRef = useRef<HTMLInputElement>(null);
  useEffect(() => {
    if (!inputRef.current) {
      return () => {};
    }
    const mousetrap = Mousetrap(inputRef.current);
    mousetrap.bind('backspace', (e) => {
      if (newChip.length === 0) {
        removeLastChip();
        return false;
      }
      return true;
    });
    mousetrap.bind('tab', (e) => {
      addNewChip();
      return false;
    });
    mousetrap.bind('space', (e) => {
      if (props.breakOnSpace) {
        addNewChip();
        return false;
      }
      return true;
    });
    return () => {
      mousetrap.unbind('tab');
      mousetrap.unbind('backspace');
      mousetrap.unbind('space');
    };
  }, [inputRef.current, addNewChip, newChip]);

  return (
    <Col gapY="2">
      <Label htmlFor={id}>{label}</Label>
      {caption && <Label gray>{caption}</Label>}
      <Row
        border="1"
        borderColor="washedGray"
        borderRadius="1"
        pl="2"
        gapX="2"
        width="100%"
        flexWrap="wrap"
        minHeight="32px"
      >
        {value.map((c, idx) => (
          <Chip key={idx}>{c}</Chip>
        ))}
        <Input
          width="auto"
          height="24px"
          flexShrink={1}
          flexGrow={1}
          pl="0"
          ref={inputRef}
          onChange={onChange}
          value={newChip}
          onBlur={onBlur}
          placeholder={placeholder}
          border="0"
          my="1"
          py="1"
        />
      </Row>
      <ErrorLabel mt="2" hasError={Boolean(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>

    </Col>
  );
}
