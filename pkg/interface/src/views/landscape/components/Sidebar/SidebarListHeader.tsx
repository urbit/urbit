import {
    Box,

    Col, Icon,

    ManagedCheckboxField as Checkbox, ManagedRadioButtonField as Radio, Row,

    Text
} from '@tlon/indigo-react';
import { FormikHelpers } from 'formik';
import React, { ReactElement, useCallback } from 'react';
import { Link, useHistory } from 'react-router-dom';
import { roleForShip } from '@urbit/api';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import { Workspace } from '~/types/workspace';
import { Dropdown } from '~/views/components/Dropdown';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { NewChannel } from '~/views/landscape/components/NewChannel';
import { SidebarListConfig } from './types';
import { getFeedPath } from '~/logic/lib/util';

export function SidebarListHeader(props: {
  initialValues: SidebarListConfig;
  baseUrl: string;
  selected: string;
  workspace: Workspace;
  handleSubmit: (s: any) => void;
}): ReactElement {
  const history = useHistory();
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

  const metadata = associations?.groups?.[groupPath]?.metadata;
  const memberMetadata =
    groupPath && metadata ? metadata.vip === 'member-metadata' : false;

  const isAdmin = memberMetadata || (role === 'admin') || (props.workspace?.type === 'home') || (props.workspace?.type === 'messages');

  const noun = (props.workspace?.type === 'messages') ? 'Messages' : 'Channels';

  const feedPath = groupPath ? getFeedPath(associations.groups[groupPath]) : undefined;

  const unreadCount = useHarkState(
    s => feedPath
    ? (s.unreads?.[`/graph/${feedPath.slice(6)}` ?? '']?.count as number ?? 0)
    : 0
  );

  return (
    <Box>
    {( feedPath) ? (
       <Row
         flexShrink={0}
         alignItems="center"
         justifyContent="space-between"
         py={2}
         px={3}
         height='48px'
         borderBottom={1}
         borderColor="lightGray"
         backgroundColor={['transparent',
           history.location.pathname.includes(`/~landscape${groupPath}/feed`)
           ? (
            'washedGray'
           ) : (
            'transparent'
           )]}
           cursor={(
             history.location.pathname === `/~landscape${groupPath}/feed`
             ? 'default' : 'pointer'
           )}
         onClick={() => {
           history.push(`/~landscape${groupPath}/feed`);
         }}
       >
         <Text>
           Group Feed
         </Text>
         <Text mr={1} color="blue">
           { unreadCount > 0 && unreadCount}
         </Text>
       </Row>
     ) : null
    }
    <Row
      flexShrink={0}
      alignItems="center"
      justifyContent="space-between"
      py={2}
      px={3}
      height='48px'
    >
      <Box flexShrink={0}>
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
              <NewChannel workspace={props.workspace} />
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
        flexShrink={0}
        width="auto"
        alignY="top"
        alignX={['right', 'left']}
        options={
          <FormikOnBlur initialValues={props.initialValues} onSubmit={onSubmit}>
            <Col bg="white" borderRadius={1} border={1} borderColor="lightGray">
              <Col
                gapY={2}
                borderBottom={1}
                borderBottomColor="washedGray"
                p={2}
              >
                <Box>
                  <Text color="gray">Sort Order</Text>
                </Box>
                <Radio mb={1} label="A -> Z" id="asc" name="sortBy" />
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
    </Box>
  );
}
