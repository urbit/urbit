import React, { ReactElement, useCallback } from 'react';
import { FormikHelpers } from 'formik';
import { Link } from 'react-router-dom';

import {
  Row,
  Box,
  Icon,
  ManagedRadioButtonField as Radio,
  ManagedCheckboxField as Checkbox,
  Col,
  Text
} from '@tlon/indigo-react';
import { Groups, Rolodex, Associations } from '@urbit/api';

import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { Dropdown } from '~/views/components/Dropdown';
import { SidebarListConfig  } from './types';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import { roleForShip } from '~/logic/lib/group';
import { NewChannel } from '~/views/landscape/components/NewChannel';
import GlobalApi from '~/logic/api/global';
import { Workspace } from '~/types/workspace';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';

export function SidebarListHeader(props: {
  api: GlobalApi;
  initialValues: SidebarListConfig;
  baseUrl: string;
  selected: string;
  workspace: Workspace;
  handleSubmit: (c: SidebarListConfig) => void;
}): ReactElement {
  const onSubmit = useCallback(
    (values: SidebarListConfig, actions: FormikHelpers<SidebarListConfig>) => {
      props.handleSubmit(values);
      actions.setSubmitting(false);
    },
    [props.handleSubmit]
  );
  const groups = useGroupState(state => state.groups);

  const groupPath = getGroupFromWorkspace(props.workspace);
  const role = groupPath && groups?.[groupPath] ? roleForShip(groups[groupPath], window.ship) : undefined;
  const associations = useMetadataState(state => state.associations);
  const memberMetadata =
    groupPath ? associations.groups?.[groupPath].metadata.vip === 'member-metadata' : false;

  const isAdmin = memberMetadata || (role === 'admin') || (props.workspace?.type === 'home') || (props.workspace?.type === 'messages');

  const noun = (props.workspace?.type === 'messages') ? 'Messages' : 'Channels';

  return (
    <Row
      flexShrink="0"
      alignItems="center"
      justifyContent="space-between"
      py={2}
      px={3}
      height='48px'
    >
      <Box flexShrink='0'>
        <Text>
          {props.initialValues.hideUnjoined ? `Joined ${noun}` : `All ${noun}`}
        </Text>
      </Box>
      <Box
        textAlign='right'
        display='flex'
        alignItems='center'
      >
        {props.workspace?.type === 'messages'
        ? (
          <Dropdown
            flexShrink={0}
            dropWidth="300px"
            width="auto"
            alignY="top"
            alignX={['right', 'left']}
            options={
              <Col
                background="white"
                border={1}
                borderColor="washedGray"
              >
              <NewChannel
                api={props.api}
                history={props.history}
                workspace={props.workspace}
              />
              </Col>
            }
          >
           <Icon icon="Plus" color="gray" pr='12px' />
          </Dropdown>
        )
        : (
       <Link style={{
          display: isAdmin ? 'inline-block' : 'none' }}
        to={groupPath
          ? `/~landscape${groupPath}/new`
          : `/~landscape/${props.workspace?.type}/new`}
       >
           <Icon icon="Plus" color="gray" pr='12px' />
       </Link>
          )
        }
      <Dropdown
        flexShrink='0'
        width="auto"
        alignY="top"
        alignX={['right', 'left']}
        options={
          <FormikOnBlur initialValues={props.initialValues} onSubmit={onSubmit}>
            <Col bg="white" borderRadius={1} border={1} borderColor="lightGray">
              <Col
                gapY="2"
                borderBottom={1}
                borderBottomColor="washedGray"
                p={2}
              >
                <Box>
                  <Text color="gray">Sort Order</Text>
                </Box>
                <Radio mb="1" label="A -> Z" id="asc" name="sortBy" />
                <Radio label="Last Updated" id="lastUpdated" name="sortBy" />
              </Col>
              <Col px={2}>
                <Checkbox
                  my={3}
                  id="hideUnjoined"
                  label="Hide Unsubscribed Channels"
                />
              </Col>
            </Col>
          </FormikOnBlur>
        }
      >
        <Icon color="gray" icon="Adjust" />
      </Dropdown>
      </Box>
    </Row>
  );
}
