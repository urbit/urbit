import React, { Component } from 'react';
import { Box, Text } from '@tlon/indigo-react';
import { ShipSearch } from '~/views/components/ShipSearch';
import { Formik, Form } from 'formik';
import { resourceFromPath } from '~/logic/lib/group';
import { AsyncButton } from '~/views/components/AsyncButton';

export class Writers extends Component {
  render() {
    const { association, groups, api } = this.props;

    const resource = resourceFromPath(association?.group);

    const onSubmit = async (values, actions) => {
      try {
        const ships = values.ships.map(e => `~${e}`);
        await api.groups.addTag(
          resource,
          { app: 'graph', resource: association.resource, tag: `writers` },
          ships
        );
        actions.resetForm();
        actions.setStatus({ success: null });
      } catch (e) {
        console.error(e);
        actions.setStatus({ error: e.message });
      }
    };
    const writers = Array.from(groups?.[association?.group]?.tags.graph[association.resource]?.writers || []).map(s => `~${s}`).join(', ');


    return (
      <Box maxWidth='512px'>
        <Text display='block'>Writers</Text>
        <Text display='block' mt='2' gray>Add additional writers to this notebook</Text>
        <Formik
          initialValues={{ ships: [] }}
          onSubmit={onSubmit}
        >
          <Form>
            <ShipSearch
              id="ships"
              label=""
              maxLength={undefined}
            />
            <AsyncButton width='100%' mt='3' primary>
            Submit
            </AsyncButton>
          </Form>
        </Formik>
        {writers.length > 0 ? <>
        <Text display='block' mt='2'>Current writers:</Text>
        <Text mt='2' display='block' mono>{writers}</Text>
        </> : 
          <Text display='block' mt='2'>
            All group members can write to this channel
          </Text>
        }
      </Box>
    );
  }
}

export default Writers;
