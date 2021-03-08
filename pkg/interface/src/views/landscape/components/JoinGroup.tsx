import React, { useState, useCallback, useEffect, ReactElement } from 'react';
import _ from 'lodash';
import { Formik, Form, FormikHelpers, useFormikContext } from 'formik';
import * as Yup from 'yup';
import { useHistory } from 'react-router-dom';
import urbitOb from 'urbit-ob';

import {
  Col,
  Row,
  Icon,
  Box,
  Text,
  ManagedTextInputField as Input
} from '@tlon/indigo-react';
import { Groups, MetadataUpdatePreview, Associations } from '@urbit/api';

import { AsyncButton } from '~/views/components/AsyncButton';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import GlobalApi from '~/logic/api/global';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import { getModuleIcon } from '~/logic/lib/util';
import { FormError } from '~/views/components/FormError';
import { GroupSummary } from './GroupSummary';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import {TUTORIAL_GROUP_RESOURCE} from '~/logic/lib/tutorialModal';

const formSchema = Yup.object({
  group: Yup.string()
    .required('Must provide group to join')
    .test('is-valid', 'Invalid group', (group: string | null | undefined) => {
      if (!group) {
        return false;
      }
      const [patp, name] = group.split('/');
      return urbitOb.isValidPatp(patp) && name.length > 0;
    })
});

interface FormSchema {
  group: string;
}

interface JoinGroupProps {
  api: GlobalApi;
  autojoin?: string;
}

function Autojoin(props: { autojoin: string | null }) {
  const { submitForm } = useFormikContext();

  useEffect(() => {
    if (props.autojoin) {
      submitForm();
    }
  }, []);

  return null;
}

export function JoinGroup(props: JoinGroupProps): ReactElement {
  const { api, autojoin } = props;
  const associations = useMetadataState(state => state.associations);
  const groups = useGroupState(state => state.groups);
  const history = useHistory();
  const initialValues: FormSchema = {
    group: autojoin || ''
  };
  const [preview, setPreview] = useState<
    MetadataUpdatePreview | string | null
  >(null);

  const waiter = useWaitForProps({ associations, groups }, _.isString(preview) ? 1 : 5000);

  const onConfirm = useCallback(async (group: string) => {
    const [,,ship,name] = group.split('/');
    if(group === TUTORIAL_GROUP_RESOURCE) {
      await api.settings.putEntry('tutorial', 'joined', Date.now());
    }
    await api.groups.join(ship, name);
    try {
      await waiter((p) => {
        return group in p.groups &&
          (group in (p.associations?.graph ?? {})
            || group in (p.associations?.groups ?? {}));
      });

      if(groups?.[group]?.hidden) {
        const { metadata } = associations.graph[group];
        history.push(`/~landscape/home/resource/${metadata.module}${group}`);
        return;
      } else {
        history.push(`/~landscape${group}`);
      }
    } catch (e) {
      //  drop them into inbox to show join request still pending
      history.push('/~notifications');
    }
  }, [api, waiter, history, associations, groups]);

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      const [ship, name] = values.group.split('/');
      const path = `/ship/${ship}/${name}`;
      //  skip if it's unmanaged
      try {
        const prev = await api.metadata.preview(path);
        actions.setStatus({ success: null });
        setPreview(prev);
      } catch (e) {
        if (!(e instanceof Error)) {
          actions.setStatus({ error: 'Unknown error' });
        } else if (e.message === 'no-permissions') {
          actions.setStatus({
            error:
              'Unable to join group, you do not have the correct permissions'
          });
        } else if (e.message === 'offline') {
          setPreview(path);
        }
      }
    },
    [api, waiter, history, onConfirm]
  );

  return (
    <Col p="3">
      <Box mb={3}>
        <Text fontSize="2" fontWeight="bold">
          Join a Group
        </Text>
      </Box>
      {_.isString(preview) ? (
        <Col width="100%" gapY="4">
          <Text>The host appears to be offline. Join anyway?</Text>
          <StatelessAsyncButton
            primary
            name="join"
            onClick={() => onConfirm(preview)}
          >
            Join anyway
          </StatelessAsyncButton>
        </Col>
      ) : preview ? (
        <GroupSummary
          metadata={preview.metadata}
          memberCount={preview?.members}
          channelCount={preview?.['channel-count']}
        >
          { Object.keys(preview.channels).length > 0 && (
            <Col
              gapY="2"
              p="2"
              borderRadius="2"
              border="1"
              borderColor="washedGray"
              bg="washedBlue"
              maxHeight="300px"
              overflowY="auto"
            >
              <Text gray fontSize="1">
                Channels
              </Text>
              <Box width="100%" flexShrink="0">
                {Object.values(preview.channels).map(({ metadata }: any) => (
                  <Row width="100%">
                    <Icon
                      mr="2"
                      color="blue"
                      icon={getModuleIcon(metadata.module) as any}
                    />
                    <Text color="blue">{metadata.title} </Text>
                  </Row>
                ))}
                </Box>
            </Col>
          )}
          <StatelessAsyncButton
            primary
            name="join"
            onClick={() => onConfirm(preview.group)}
          >
            Join {preview.metadata.title}
          </StatelessAsyncButton>
        </GroupSummary>
      ) : (
        <Col width="100%" gapY="4">
          <Formik
            validationSchema={formSchema}
            initialValues={initialValues}
            onSubmit={onSubmit}
          >
            <Form style={{ display: 'contents' }}>
              <Autojoin autojoin={autojoin ?? null} />
              <Input
                id="group"
                label="Group"
                caption="What group are you joining?"
                placeholder="~sampel-palnet/test-group"
              />
              <AsyncButton mt="4">Join Group</AsyncButton>
              <FormError mt="4" />
            </Form>
          </Formik>
        </Col>
      )}
    </Col>
  );
}
