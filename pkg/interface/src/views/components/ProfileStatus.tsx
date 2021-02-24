import React, { useState, useCallback, useEffect } from 'react';

import {
  Row,
  StatelessTextInput as Input,
  Text
} from "@tlon/indigo-react";
import { editContact } from '@urbit/api';

import useApi from '~/logic/lib/useApi';

export const ProfileStatus = (props) => {
  const { contact, ship, callback } = props;
  const [_status, setStatus] = useState('');
  const [notice, setNotice] = useState(' ');
  const api = useApi();

  const onStatusChange = useCallback(
    (e) => {
      setStatus(e.target.value);
    },
    [setStatus]
  );

  useEffect(() => {
    setStatus(!!contact ? contact.status : '');
  }, [contact]);

  const editStatus = () => {
    api.poke(editContact(ship, { status: _status }));

    setNotice('Success!');
    setTimeout(() => {
      setNotice(' ');
    }, 1000);

    if (callback) {
      callback();
    }
  };

  return (
    <>
      <Row width='100%' mt={1}>
        <Input
          onChange={onStatusChange}
          value={_status}
          autocomplete='off'
          width='100%'
          placeholder='Set Status'
          onKeyPress={(evt) => {
            if (evt.key === 'Enter') {
              editStatus();
            }
          }}
          onBlur={() => {
            editStatus();
          }}
        />
      </Row>
      <Text
        display={notice !== ' ' ? 'block' : 'none'}
        mt={1}
        ml={1}
        whiteSpace='pre'
        gray
      >
        {notice}
      </Text>
    </>
  );
};
