import { useEffect, useState } from 'react'
import './App.css'

import Button from '@mui/material/Button';
import { Box, CssBaseline, TextField, Typography } from '@mui/material';

import init, { parse_sql } from "./postgresql-lst-parser/postgresql_lst_parser_wasm.js";

function App() {
  const [sql, setSQL] = useState('SELECT\n\ttbl.a as a\nfrom\n\tTBL tbl')
  const [parseResult, setParseResult] = useState('')

  useEffect(() => {
    (async () => {
      await init();
      setParseResult(parse_sql(sql))
    })()
  }, [])

  return (
    <>
      <CssBaseline />
      <Box sx={{ display: 'flex', flexDirection: 'column', padding: '16px', height: '100vh' }}>
        <Box sx={{ mb: 1 }}>
          <Typography
            component="h1"
            variant="h6"
            sx={{ p: 1, flexGrow: 1 }}
            align="left"
          >PostgreSQL LST Parser
          </Typography>
        </Box>

        <Box sx={{ mb: 3, display: 'flex', flexDirection: 'row', alignContent: 'flex-start' }}>
          <Button variant="contained" color="primary" onClick={() => setParseResult(parse_sql(sql))}>
            Parse
          </Button>
        </Box>

        <Box sx={{ display: 'flex', flexDirection: 'row', justifyContent: 'space-between', flexGrow: 1 }}>
          <Box sx={{ display: 'flex', flexDirection: 'column', flex: 1 }}>
            <TextField
              multiline
              fullWidth
              variant="outlined"
              label="SQL"
              defaultValue={sql}
              onChange={e => setSQL(e.target.value)}
              sx={{ flex: 1 }}
              InputProps={{
                style: { flex: 1, alignItems: 'flex-start' }
              }}
            />
          </Box>
          <Box sx={{ width: '16px' }}></Box>
          <Box sx={{ display: 'flex', flexDirection: 'column', flex: 1 }}>
            <TextField
              multiline
              fullWidth
              variant="outlined"
              label="Parse Result"
              sx={{
                flex: 1,
                '& .MuiInputBase-inputMultiline': {
                  height: '100%',
                }
              }}
              InputProps={{
                inputComponent: 'textarea',
                style: { flex: 1, alignItems: 'flex-start' },
              }}
              value={parseResult}
            />
          </Box>
        </Box>
      </Box >
    </>
  )
}

export default App
