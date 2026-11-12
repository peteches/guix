// strata-soak.js — load soak for the strata engine (see peteches/services/strata.scm).
// 41 mixed requests incl. concurrent waves; detects the #606 single-token loop.
// Run after ANY engine/config change: node scripts/strata-soak.js [base-url]
// (default base https://strata.ts.peteches.co.uk; exit 0 = clean).
// strata soak: 20 sequential + 18 concurrent (waves of 3) + 3 tail requests.
// Flags any '!'-run (>=8 consecutive) in content or reasoning. Usage: node soak.js [base-url]
const BASE=process.argv[2]||"https://strata.ts.peteches.co.uk";
const https=require("https");
process.env.NODE_TLS_REJECT_UNAUTHORIZED="0";
function post(body,timeoutMs=900000){return new Promise((res,rej)=>{
  const data=JSON.stringify(body);const u=new URL(BASE+"/v1/chat/completions");
  const req=https.request({hostname:u.hostname,path:u.pathname,method:"POST",
    headers:{"Content-Type":"application/json","Content-Length":Buffer.byteLength(data)}},r=>{
    let b="";r.on("data",c=>b+=c);r.on("end",()=>{try{res(JSON.parse(b))}catch(e){rej(new Error("badjson:"+b.slice(0,200)))}});});
  req.setTimeout(timeoutMs,()=>req.destroy(new Error("timeout")));req.on("error",rej);req.end(data);});}
const chat=(content,mt)=>post({model:"strata",messages:[{role:"user",content}],max_tokens:mt,temperature:0.8});
const bang=s=>/(!)\1{7,}/.test(s||"");
const prompts=[
 ["In one sentence, what is a closure in programming?",512],
 ["Write a haiku about a stuck elevator.",512],
 ["What is 23 * 17? Show one line of reasoning then the answer.",640],
 ["List 5 sorting algorithms with one-line complexity notes.",1024],
 ["Explain quicksort to a 10-year-old in 3 sentences.",640],
 ["Write a limerick about a sysadmin named Gus.",512],
 ["Translate to French: The weather in Edinburgh is unpredictable.",512],
 ["Give a JSON example of a user record with 3 fields. JSON only.",640],
 ["What does the 'set -e' shell option do? One sentence.",512],
 ["Name 4 Shakespeare plays and one character from each.",1024],
 ["Write a Python one-liner that reverses words in a string.",640],
 ["Summarize the plot of Macbeth in two sentences.",640],
 ["What is the difference between TCP and UDP? Two sentences.",640],
 ["Count backwards from 20 to 1 separated by dashes.",512],
 ["Write 4 lines of dialogue between a robot and a toaster.",1024],
 ["Explain what a GPU does in one short paragraph.",640],
 ["Give me three dinner ideas using chickpeas.",1024],
 ["What is 2^10? Just the number and one word of explanation.",512],
 ["Write a two-sentence horror story.",640],
 ["Describe the color blue to someone who cannot see.",1024]];
async function one(i){
  const [p,mt]=prompts[i%prompts.length];
  try{
    const r=await chat(p+(i>=20?` (variant ${i})`:""),mt);
    const c=r.choices[0];const txt=(c.message.content||"")+" "+(c.message.reasoning_content||"");
    return {i,ok:!bang(txt),bang:bang(txt),len:c.finish_reason==="length",
      tok:r.usage?r.usage.completion_tokens:0,fr:c.finish_reason};
  }catch(e){return {i,ok:false,err:e.message.slice(0,80)};}
}
async function main(){
  const t0=Date.now();const results=[];
  for(let i=0;i<20;i++){const r=await one(i);results.push(r);if(!r.ok)console.log("BAD",JSON.stringify(r));}
  console.log(`sequential 20 done (${((Date.now()-t0)/1000).toFixed(0)}s)`);
  const t1=Date.now();
  for(let w=0;w<6;w++){
    const rs=await Promise.all([one(20+w*3),one(21+w*3),one(22+w*3)]);
    results.push(...rs);rs.forEach(r=>{if(!r.ok)console.log("BAD",JSON.stringify(r))});
  }
  console.log(`load waves done (${((Date.now()-t1)/1000).toFixed(0)}s)`);
  for(let i=38;i<41;i++){results.push(await one(i));}
  const bad=results.filter(r=>!r.ok).length;
  const bangs=results.filter(r=>r.bang).length;
  const errs=results.filter(r=>r.err).length;
  const toks=results.reduce((s,r)=>s+(r.tok||0),0);
  console.log(`\nSOAK SUMMARY: ${results.length} requests, bad=${bad} bangloops=${bangs} errors=${errs} total_completion_tokens=${toks} wall=${((Date.now()-t0)/1000).toFixed(0)}s`);
  process.exit(bad>0?1:0);
}
main().catch(e=>{console.error("SOAK-ERROR",e.message);process.exit(1)});
