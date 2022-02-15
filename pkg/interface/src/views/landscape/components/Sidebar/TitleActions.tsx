import {
  Box, Button, Col, Icon, Text, Row,
  ManagedCheckboxField as Checkbox, ManagedRadioButtonField as Radio
} from '@tlon/indigo-react';
import { FormikHelpers } from 'formik';
import React, { ReactElement, useCallback } from 'react';
import { Link } from 'react-router-dom';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import { Workspace } from '~/types/workspace';
import { Dropdown } from '~/views/components/Dropdown';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { StatusBarItem } from '~/views/components/StatusBarItem';
import { NewChannel } from '~/views/landscape/components/NewChannel';
import { bootstrapApi } from '~/logic/api/bootstrap';
import { SidebarListConfig } from './types';
import { IS_MOBILE } from '~/logic/lib/platform';

export function TitleActions(props: {
  initialValues: SidebarListConfig;
  baseUrl: string;
  workspace: Workspace;
  handleSubmit: (s: any) => void;
  toggleChangingSort: () => void;
}): ReactElement {
  const onSubmit = useCallback(
    (values: SidebarListConfig, actions: FormikHelpers<SidebarListConfig>) => {
      props.handleSubmit(values);
      actions.setSubmitting(false);
    },
    [props.handleSubmit]
  );
  const groupPath = getGroupFromWorkspace(props.workspace);
  const isHome = props.workspace.type === 'uqbar-home';
  const refreshConnections = useCallback(() => {
    bootstrapApi(true);
  }, []);

  return (
    <Row
      flexShrink={0}
      alignItems="center"
      justifyContent="space-between"
      py={2}
      height='48px'
      onClick={(e) => {
        e.preventDefault();
        e.stopPropagation();
      }}
    >
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
        : !isHome && (
          <Link style={{ display: 'inline-block' }}
            to={groupPath
              ? `/~landscape${groupPath}/new`
              : `/~landscape/${props.workspace?.type}/new`}
          >
              <Icon icon="Plus" color="gray" pr='12px' />
          </Link>
          )
        }
      {isHome ? (
        <>
          <Button onClick={props.toggleChangingSort}>Order {IS_MOBILE ? '' : 'Groups'}</Button>
          {IS_MOBILE && (
            <StatusBarItem
              as={Button}
              width='32px'
              ml={2}
              onClick={refreshConnections}
            >
              <Icon icon='ArrowRefresh' />
            </StatusBarItem>
          )}
        </>
      ) : (
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
      )}
      </Box>
    </Row>
  );
}
