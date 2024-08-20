import { Box, Text } from '@tlon/indigo-react';
import { addTag, Association, Group, resourceFromPath } from '@urbit/api';
import { Form, Formik } from 'formik';
import React, { ReactElement } from 'react';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ShipSearch } from '~/views/components/ShipSearch';
import airlock from '~/logic/api';

interface WritersProps {
  association: Association;
  groups: Group[];
}

export const Writers = (props: WritersProps): ReactElement => {
  const { association, groups } = props;

    const resource = resourceFromPath(association?.group);

    const onSubmit = async (values, actions) => {
      try {
        const ships = values.ships.map(e => `~${e}`);
        await airlock.poke(addTag(
          resource,
          { app: 'graph', resource: association.resource, tag: 'writers' },
          ships
        ));
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
        <Text display='block' mt={2} gray>Add additional writers to this notebook</Text>
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
            <AsyncButton width='100%' mt={3} primary>
            Submit
            </AsyncButton>
          </Form>
        </Formik>
        {writers.length > 0 ? <>
        <Text display='block' mt={2}>Current writers:</Text>
        <Text mt={2} display='block' mono>{writers}</Text>
        </> :
          <Text display='block' mt={2}>
            All group members can write to this channel
          </Text>
        }
      </Box>
    );
};

export default Writers;
