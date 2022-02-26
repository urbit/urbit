import React, { useCallback, useEffect, useState } from 'react';

import * as Yup from 'yup';
import { Form, Formik, FormikHelpers } from 'formik';
import { Box, Button, Icon, IconIndex, LoadingSpinner, Row, Text, ManagedTextInputField as Input } from '@tlon/indigo-react';

import useDocketState from '~/logic/state/docket';
import usePalsState from '~/logic/state/pals';
import { PALS_APP, PALS_HOST } from '~/logic/constants/install';
import { ModalOverlay } from '../ModalOverlay';
import { ActionRow } from '../ProfileOverlay';

const formSchema = Yup.object({
  tag: Yup.string().required('Tag cannot be blank').max(20, 'Max 20 characters')
});

export interface TagFormSchema {
  tag: string;
}

interface PalsProfileInfoProps {
  ship: string;
}

export const PalsProfileInfo = ({
  ship
}: PalsProfileInfoProps) => {
  const { addAlly, requestTreaty, installDocket } = useDocketState();
  const { installed, loading, pals, mutuals, fetchPals, addPal, removePal, set: setPalsState } = usePalsState();
  const [isInstalling, setIsInstalling] = useState(false);
  const [showInstallModal, setShowInstallModal] = useState(false);
  const [tags, setTags] = useState(pals.outgoing[ship]?.lists || []);

  const isMutual = Boolean(mutuals[ship]);
  const isLeech = !isMutual && Boolean(pals.outgoing[ship]);
  const isTarget = !isMutual && Boolean(pals.incoming[ship]);

  useEffect(() => {
    fetchPals();
  }, []);

  const handleSendRequest = useCallback(() => {
    if (!loading && installed) {
      addPal(ship);
    } else {
      setShowInstallModal(true);
    }
  }, [loading, installed, ship]);

  const handleInstall = useCallback(async () => {
    setIsInstalling(true);
    setShowInstallModal(false);
    try {
      await addAlly(PALS_HOST);
      await requestTreaty(PALS_HOST, PALS_APP);
      await installDocket(PALS_HOST, PALS_APP);
      setPalsState({ installed: true });
    } catch (err) {
      console.warn('PALS INSTALL ERROR:', err);
    } finally {
      setIsInstalling(false);
    }
  }, [setIsInstalling, setShowInstallModal, addAlly, requestTreaty, installDocket, setPalsState]);

  const removeTag = useCallback(async (tag: string) => {
    try {
      const newTags = tags.filter(t => t !== tag);
      await addPal(ship, newTags);
      setTags(newTags);
    } catch (e) {
      console.error(e);
    }
  }, [ship, tags]);

  const onSubmitTag = useCallback(async (values: TagFormSchema, actions: FormikHelpers<TagFormSchema>) => {
    try {
      actions.setSubmitting(true);
      const { tag } = values;
      const newTags = tags.concat([tag]);
      await addPal(ship, newTags);
      setTags(newTags);
      actions.resetForm();
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: 'Failed to save tag' });
    }
    actions.setSubmitting(false);
  }, [ship, tags]);

  const getActionProps = () : { icon: keyof IconIndex; text: string | Element; onClick?: () => void } => {
    if (isMutual || isLeech) {
      return { icon: 'Delete', text: 'Remove from pals', onClick: () => removePal(ship) };
    // } else if (isLeech) {
    //   return { icon: 'Clock', text: 'Pals request pending' };
    } else if (isTarget) {
      return { icon: 'AddUser', text: 'Add to pals', onClick: () => addPal(ship) };
    }

    // TODO: if %pals isn't installed, show a modal with the option to install %pals
    return { icon: 'AddUser', text: 'Add to pals', onClick: handleSendRequest };
  };

  if (showInstallModal) {
    return (
      <ModalOverlay
        bg="transparent"
        height="100%"
        width="100%"
        display="flex"
        flexDirection="column"
        justifyContent="center"
        alignItems="center"
        dismiss={() => setShowInstallModal(false)}
      >
        <Box backgroundColor="white" p={4} borderRadius={3} display="flex" flexDirection="column" alignItems="center">
          <Text>You do not have <Text mono>%pals</Text> installed,<br /> would you like to install it?</Text>
          <Row mt={3}>
            <Button color="white" backgroundColor="black" onClick={() => setShowInstallModal(false)} >Cancel</Button>
            <Button ml={2} onClick={handleInstall}>Install</Button>
          </Row>
        </Box>
      </ModalOverlay>
    );
  } else if (isInstalling) {
    return (
      <ActionRow width="80%">
        <LoadingSpinner />
        <Text ml={2}>Installing...</Text>
      </ActionRow>
    );
  }

  const { icon, text, onClick } = getActionProps();

  return (
    <>
      <ActionRow width="100%" onClick={onClick}>
        <Icon icon={icon} size={16} mr={2} />
        <Text>{text}</Text>
      </ActionRow>
      {tags.length > 0 && (
        <Row flexWrap="wrap" my={1}>
          {tags.map(tag => (
            <Box key={tag}
              backgroundColor="washedGray"
              px={2} py={1}
              m={1} borderRadius={3}
              onClick={() => removeTag(tag)} display="flex"
              cursor="pointer"
            >
              <Text>{tag}</Text>
              <Icon ml={1} icon="X" alignSelf="center" />
            </Box>
          ))}
        </Row>
      )}
      {((isMutual || isLeech) && tags.length < 4) && (
        <Box mt={1}>
          <Formik
            validationSchema={formSchema}
            initialValues={{ tag: '' }}
            onSubmit={onSubmitTag}
          >
            <Form style={{ display: 'contents' }}>
              <Row flexShrink={0} flexDirection={['column-reverse', 'row']} gapX={2} justifyContent='space-between'>
                <Input maxWidth='24rem' width='100%' placeholder="Add pals tag" id="tag" />
                <Button>Add</Button>
              </Row>
            </Form>
          </Formik>
        </Box>
      )}
    </>
  );
};
