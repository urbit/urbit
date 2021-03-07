import React, {
  useState,
  useCallback,
  useEffect,
  ChangeEvent,
  useRef
} from 'react';
import {
  Row,
  Text,
  Button,
  StatelessTextInput as Input
} from '@tlon/indigo-react';

export function SetStatus(props: any) {
  const { contact, ship, api, callback, smallControl } = props;
  const inputRef = useRef(null);
  const [_status, setStatus] = useState('');
  const onStatusChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      setStatus(e.target.value);
    },
    [setStatus]
  );

  useEffect(() => {
    setStatus(contact ? contact.status : '');
  }, [contact]);

  const editStatus = () => {
    api.contacts.edit(ship, { status: _status });
    inputRef.current.blur();
    if (callback) {
      callback();
    }
  };

  if (smallControl) {
    return (
      <>
        <Row mb='2'>
          <Text color='gray' fontWeight='500'>
            Set Status:
          </Text>
        </Row>
        <Row>
          <Input
            ref={inputRef}
            onChange={onStatusChange}
            value={_status}
            autocomplete='off'
            width='100%'
            onKeyPress={(evt) => {
              if (evt.key === 'Enter') {
                editStatus();
              }
            }}
          />
        </Row>
      </>
    );
  }

  return (
    <Row width='100%' my={3}>
      <Input
        ref={inputRef}
        onChange={onStatusChange}
        value={_status}
        autocomplete='off'
        width='75%'
        mr={2}
        onKeyPress={(evt) => {
          if (evt.key === 'Enter') {
            editStatus();
          }
        }}
      />
      <Button primary color='white' ml={2} width='25%' onClick={editStatus}>
        Set Status
      </Button>
    </Row>
  );
}
